use crate::vm::{Word, Instr};
use std::collections::HashMap;
use std::rc::Rc;
use std::borrow::Borrow;
use std::fmt;

#[derive(Debug)]
pub enum Expr {
    Const(u64),
    Var(Variable),
    Let(Box<Expr>, Box<Expr>),
    Lam(Type, Box<Expr>),
    App(Box<Expr>, Box<Expr>, Vec<Box<Expr>>),

    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Variable {
    Free(String),
    Bounded(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nat,
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    pub fn is_arrow(&self) -> bool {
        if let Type::Arrow(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn arrow<T: Into<Box<Type>>, S: Into<Box<Type>>>(l: T, r: S) -> Type {
        Type::Arrow(l.into(), r.into())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nat => write!(f, "Nat"),
            Type::Arrow(l, r) => if l.is_arrow() {
                write!(f, "({}) -> {}", l, r)
            } else {
                write!(f, "{} -> {}", l, r)
            },
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Rc<String>,
    pub ty: Type,
    body: Box<Expr>,
}

#[derive(Debug)]
pub enum TypeError {
    Unexpected,
    Undefined(String),
}

type TResult<T> = Result<T, TypeError>;

pub struct Gamma {
    data: Vec<Type>,
}

impl Gamma {
    // quering a bounded variable never fails
    fn get(&self, idx: u64) -> Type {
        assert_ne!(idx, 0);
        self.data[idx as usize].clone()
    }

    fn add(&mut self, t: Type) {
        self.data.push(t)
    }

    fn pop(&mut self) -> Option<Type> {
        self.data.pop()
    }

    pub fn clear(&mut self) {
        self.data.clear()
    }

    pub fn new() -> Self {
        Self { data: vec![Type::Nat] }
    }
}

impl Expr {
    pub fn typing(&self, gamma: &mut Gamma, global: &Global) -> TResult<Type> {
        match self {
            Expr::Const(_) => Ok(Type::Nat),
            Expr::Var(Variable::Bounded(i)) => Ok(gamma.get(*i)),
            Expr::Var(Variable::Free(s)) => global.try_lookup(&s[..]).map(|t| t.clone()),
            Expr::Let(e0, e1) => {
                let t0 = e0.typing(gamma, global)?;
                gamma.add(t0);
                let ret = e1.typing(gamma, global);
                gamma.pop();
                ret
            },
            Expr::Lam(t, e) => {
                gamma.add(t.clone());
                let t1 = e.typing(gamma, global)?;
                let t0 = gamma.pop().unwrap();
                Ok(Type::Arrow(Box::new(t0), Box::new(t1)))
            },
            Expr::App(f, arg, args) => {
                let mut t_f = &f.typing(gamma, global)?;
                let mut t_x = arg.typing(gamma, global)?;
                let mut args = args.iter();
                loop {
                    match t_f {
                        Type::Arrow(t0, t1) if &**t0 == &t_x => {
                            if let Some(e) = args.next() {
                                t_f = &*t1;
                                t_x = e.typing(gamma, global)?;
                            } else {
                                return Ok((**t1).clone());
                            };
                        },
                        _ => return Err(TypeError::Unexpected),
                    }
                }
            },

            Expr::Add(e0, e1) => {
                let t0 = e0.typing(gamma, global)?;
                if t0 != Type::Nat {
                    return Err(TypeError::Unexpected);
                }
                let t1 = e1.typing(gamma, global)?;
                if t1 == Type::Nat {
                    Ok(Type::Nat)
                } else {
                    Err(TypeError::Unexpected)
                }
            },
        }
    }

    pub fn compile_non_tail(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) -> u64 {
        match self {
            Expr::Const(v) => {
                let inst = Instr::Load(Word::try_from(*v).unwrap());
                out.push(inst);
                1
            },
            Expr::Var(Variable::Bounded(i)) => {
                let inst = Instr::Fetch(Word(i - 1));
                out.push(inst);
                1
            },
            Expr::Var(Variable::Free(s)) => {
                let addr = name_table.lookup(&s[..]);
                let inst = Instr::MkClosure(*addr);
                out.push(inst);
                1
            },
            Expr::Let(e0, e1) => {
                let l0 = e0.compile_non_tail(name_table, out);
                out.push(Instr::Let);
                let l1 = e1.compile_non_tail(name_table, out);
                out.push(Instr::EndLet);
                l0 + l1 + 2
            },
            Expr::Lam(_, e) => {
                let i = out.len();
                out.push(Instr::Halt);
                let l = e.compile_tail(name_table, out);
                out.push(Instr::Return);
                out[i] = Instr::LFJF(Word(l + 2));
                l + 2
            },
            Expr::App(f, arg, args) => Self::compile_app(f, arg, args, name_table, out, false),
            Expr::Add(e0, e1) => {
                let l0 = e0.compile_non_tail(name_table, out);
                out.push(Instr::PushA);
                let l1 = e1.compile_non_tail(name_table, out);
                out.push(Instr::Add);
                l0 + l1 + 2
            },
        }
    }

    fn compile_tail(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) -> u64 {
        match self {
            Expr::Let(e0, e1) => {
                let l1 = e0.compile_non_tail(name_table, out);
                out.push(Instr::Let);
                let l2 = e1.compile_tail(name_table, out);
                l1 + l2 + 1
            },
            Expr::Lam(_, e) => {
                out.push(Instr::Grab);
                let l = e.compile_tail(name_table, out);
                l + 1
            },
            Expr::App(f, arg, args) => Self::compile_app(f, arg, args, name_table, out, true),
            _ => self.compile_non_tail(name_table, out),
        }
    }

    fn compile_app(f: &Box<Expr>, arg: &Box<Expr>, args: &Vec<Box<Expr>>, 
        name_table: &NameTable<Word>, out: &mut Vec<Instr>, tail_call: bool) -> u64
    {
        out.push(Instr::PushMark);
        let mut l = 0;
        for x in args.iter().rev() {
            let l_x = x.compile_non_tail(name_table, out);
            out.push(Instr::PushA);
            l += l_x + 1;
        }
        let l0 = arg.compile_non_tail(name_table, out);
        out.push(Instr::PushA);
        let l1 = f.compile_non_tail(name_table, out);
        let inst = if tail_call {
            Instr::TailApply
        } else {
            Instr::Apply
        };
        out.push(inst);
        l + l0 + l1 + 3
    }

    pub fn compile(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) {
        let _ = self.compile_tail(name_table, out);
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct Name(Rc<String>);

impl From<Rc<String>> for Name {
    fn from(v: Rc<String>) -> Self {
        Self(v)
    }
}

impl Borrow<str> for Name {
    fn borrow(&self) -> &str {
        (*self.0).as_str()
    }
}

pub struct NameTable<T> {
    data: HashMap<Name, T>
}

impl<T> NameTable<T> {
    pub fn new() -> Self {
        NameTable { data: HashMap::new() }
    }

    fn lookup(&self, name: &str) -> &T {
        self.data.get(name).unwrap()
    }

    fn try_lookup(&self, name: &str) -> TResult<&T> {
        match self.data.get(name) {
            Some(t) => Ok(t),
            None => Err(TypeError::Undefined(name.to_string())),
        }
    }

    pub fn insert<K: Into<Name>>(&mut self, k: K, v: T) {
        let _ = self.data.insert(k.into(), v);
    }
}

type Global = NameTable<Type>;

impl Function {
    pub fn new<T: Into<Rc<String>>>(name: T, ty: Type, body: Box<Expr>) -> Function {
        Function {
            name: name.into(),
            ty,
            body,
        }
    }

    pub fn type_check(&self, gamma: &mut Gamma, global: &Global) -> TResult<()> {
        let t = self.body.typing(gamma, global)?;
        if t == self.ty {
            Ok(())
        } else {
            Err(TypeError::Unexpected)
        }
    }

    pub fn compile(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) {
        if let Expr::Lam(_, ref e) = *self.body {
            e.compile_tail(name_table, out);
            out.push(Instr::Return);
        } else {
            unreachable!()
        }
    }
}

