use crate::vm::{Word, Instr};
use std::collections::HashMap;
use std::rc::Rc;
use std::borrow::Borrow;

#[derive(Debug)]
pub enum Expr {
    Const(u64),
    Var(u64),
    Let(Box<Expr>, Box<Expr>),
    App(String, Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nat,
}

#[derive(Debug)]
pub struct Function {
    pub name: Rc<String>,
    pub ty: (Type, Type),
    body: Box<Expr>,
}

impl Function {
    pub fn new<T: Into<Rc<String>>>(name: T, body: Box<Expr>) -> Function {
        Function {
            name: name.into(),
            ty: (Type::Nat, Type::Nat),
            body,
        }
    }
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

    fn pop(&mut self) {
        let _ = self.data.pop();
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
            Expr::Var(i) => Ok(gamma.get(*i)),
            Expr::Let(e0, e1) => {
                let ty0 = e0.typing(gamma, global)?;
                gamma.add(ty0);
                let ret = e1.typing(gamma, global);
                gamma.pop();
                ret
            },
            Expr::App(f, param) => {
                let ty = param.typing(gamma, global)?;
                let (t0, t1) = global.try_lookup(&f[..])?;
                if &ty == t0 {
                    Ok(t1.clone())
                } else {
                    Err(TypeError::Unexpected)
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

    pub fn compile(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) {
        match self {
            Expr::Const(v) => {
                let inst = Instr::Load(Word(*v));
                out.push(inst);
            },
            Expr::Var(i) => {
                let inst = Instr::Fetch(Word(*i));
                out.push(inst);
            },
            Expr::Let(e0, e1) => {
                e0.compile(name_table, out);
                out.push(Instr::PushA);
                e1.compile(name_table, out);
                out.push(Instr::Pop);
            },
            Expr::App(f, param) => {
                let addr = name_table.lookup(&f[..]);
                out.push(Instr::Push(*addr));
                param.compile(name_table, out);
                out.push(Instr::Call);
            },
            Expr::Add(e0, e1) => {
                e0.compile(name_table, out);
                out.push(Instr::PushA);
                e1.compile(name_table, out);
                out.push(Instr::Add);
            },
        }
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

type Global = NameTable<(Type, Type)>;

impl Function {
    pub fn type_check(&self, gamma: &mut Gamma, global: &Global) -> TResult<()> {
        gamma.add(self.ty.0.clone());
        let t = self.body.typing(gamma, global)?;
        if t == self.ty.1 {
            Ok(())
        } else {
            Err(TypeError::Unexpected)
        }
    }

    pub fn compile(&self, name_table: &NameTable<Word>, out: &mut Vec<Instr>) {
        out.push(Instr::PushA);
        self.body.compile(name_table, out);
        out.push(Instr::Pop);
        out.push(Instr::Return);
    }
}

