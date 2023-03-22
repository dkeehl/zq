use crate::ir::{Expr, Type, Variable, Function,};
use crate::lexer::Operator;
use std::rc::Rc;
use std::fmt;

pub enum Ast {
    Int(u64),
    Ident(Identifier),
    Lam(Identifier, Type, Box<Ast>),
    App(Box<Ast>, Box<Ast>, Vec<Ast>),
    Op2(Operator, Box<Ast>, Box<Ast>),
    Let(Box<Ast>, Identifier, Box<Ast>),
}

impl Ast {
    pub fn lam<T: Into<Box<Ast>>>(s: Identifier, ty: Type, e: T) -> Ast {
        Ast::Lam(s, ty, e.into())
    }

    pub fn app<T: Into<Box<Ast>>, S: Into<Box<Ast>>>(f: T, arg: S, args: Vec<Ast>) -> Ast {
        Ast::App(f.into(), arg.into(), args)
    }

    pub fn local<T: Into<Box<Ast>>, S: Into<Box<Ast>>>(x: Identifier, e0: T, e1: S) -> Ast {
        Ast::Let(e0.into(), x, e1.into())
    }

    pub fn op2<T: Into<Box<Ast>>, S: Into<Box<Ast>>>(op: Operator, l: T, r: S) -> Ast {
        Ast::Op2(op, l.into(), r.into())
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Int(n) => write!(f, "{}", n),
            Ast::Ident(s) => write!(f, "{}", s),
            Ast::Lam(var, ty, e) => write!(f, "\\{}: {} => {}", var, ty, e),
            Ast::Op2(op, e0, e1) => write!(f, "({} {} {})", e0, op, e1),
            Ast::App(lam, arg, args) => {
                write!(f, "{} {}", lam, arg)?;
                for x in args.iter() {
                    write!(f, " {}", x)?;
                }
                Ok(())
            },
            Ast::Let(e0, name, e1) => write!(f, "let {} = {} in\n{}", name, e0, e1),
        }
    }
}

pub type Identifier = String;

#[derive(Clone)]
enum List<T> {
    Empty,
    Cons {
        head: T,
        tail: Rc<List<T>>,
    },
}

impl<'a> List<&'a str> {
    fn new(head: &'a str, tail: Rc<Self>) -> Self {
        List::Cons {
            head,
            tail,
        }
    }

    fn subst(&self, name: &str, depth: u64) -> Option<u64> {
        match self {
            List::Empty => None,
            List::Cons { head, tail } => {
                if *head == name {
                    Some(depth)
                } else {
                    tail.subst(name, depth - 1)
                }
            },
        }
    }
}

impl Ast {
    fn de_bruijn_index_prim(&self, names: Rc<List<&str>>, depth: u64) -> Box<Expr> {
        match self {
            Ast::Int(n) => Box::new(Expr::Const(*n)),
            Ast::Ident(name) => {
                match names.subst(name.as_str(), depth) {
                    Some(i) => Box::new(Expr::Var(Variable::Bounded(i))),
                    None => Box::new(Expr::Var(Variable::Free(name.clone()))),
                }
            },
            Ast::Op2(op, a0, a1) => {
                if op == &Operator::Plus {
                    let e0 = a0.de_bruijn_index_prim(names.clone(), depth);
                    let e1 = a1.de_bruijn_index_prim(names, depth);
                    Box::new(Expr::Add(e0, e1))
                } else {
                    unimplemented!()
                }
            },
            Ast::Lam(x, t, e) => {
                let names = Rc::new(List::new(x.as_str(), names));
                let e = e.de_bruijn_index_prim(names, depth + 1);
                Box::new(Expr::Lam(t.clone(), e))
            },
            Ast::App(f, arg, args) => {
                let f = f.de_bruijn_index_prim(names.clone(), depth);
                let arg = arg.de_bruijn_index_prim(names.clone(), depth);
                let args = args.iter()
                    .map(|x| x.de_bruijn_index_prim(names.clone(), depth))
                    .collect();
                Box::new(Expr::App(f, arg, args))
            },
            Ast::Let(a0, x, a1) => {
                let e0 = a0.de_bruijn_index_prim(names.clone(), depth);
                let names = Rc::new(List::new(x.as_str(), names));
                let e1 = a1.de_bruijn_index_prim(names, depth + 1);
                Box::new(Expr::Let(e0, e1))
            },
        }
    }

    pub fn de_bruijn_index(&self) -> Box<Expr> {
        let names = Rc::new(List::Empty);
        self.de_bruijn_index_prim(names, 0)
    }
}

pub struct Definition {
    f_name: Identifier,
    arg: (Identifier, Type),
    args: Vec<(Identifier, Type)>,
    ret_type: Type,
    f_body: Box<Ast>,
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fun {}(", self.f_name);
        write!(f, "{}: {}", self.arg.0, self.arg.1);
        for x in self.args.iter() {
            write!(f, ", {}: {}", x.0, x.1);
        }
        write!(f, "): {} =\n{}", self.ret_type, self.f_body)
    }
}

impl Definition {
    pub fn new<T: ToString, E: Into<Box<Ast>>>
        (t: T, arg: (Identifier, Type), args: Vec<(Identifier, Type)>, ret_type: Type, e: E)
        -> Self
    {
        Definition {
            f_name: t.to_string(),
            arg,
            args,
            ret_type,
            f_body: e.into(),
        }
    }

    pub fn de_bruijn_index(&self) -> Function {
        let mut names = Rc::new(List::new(&self.arg.0[..], Rc::new(List::Empty)));
        for (x, _) in self.args.iter() {
            names = Rc::new(List::new(&x[..], names));
        }
        let mut ty = self.ret_type.clone();
        let mut e = self.f_body.de_bruijn_index_prim(names, self.args.len() as u64 + 1);
        for (_, t) in self.args.iter().rev() {
            ty = Type::Arrow(Box::new(t.clone()), Box::new(ty));
            e = Box::new(Expr::Lam(t.clone(), e));
        }
        ty = Type::Arrow(Box::new(self.arg.1.clone()), Box::new(ty));
        e = Box::new(Expr::Lam(self.arg.1.clone(), e));
        Function::new(self.f_name.clone(), ty, e)
    }
}

pub enum Item {
    Expression(Ast),
    FunDef(Definition),
}

