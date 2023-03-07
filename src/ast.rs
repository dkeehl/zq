use crate::ir::{Expr, Function,};
use crate::lexer::Operator;
use std::rc::Rc;
use std::fmt;

pub enum Ast {
    Int(u64),
    Ident(Identifier),
    Op2(Operator, Box<Ast>, Box<Ast>),
    App(Identifier, Box<Ast>),
    Let(Box<Ast>, Identifier, Box<Ast>),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Int(n) => write!(f, "{}", n),
            Ast::Ident(s) => write!(f, "{}", s),
            Ast::Op2(op, e0, e1) => write!(f, "({} {} {})", e0, op, e1),
            Ast::App(f_name, arg) => write!(f, "{} {}", f_name, arg),
            Ast::Let(e0, name, e1) => write!(f, "let {} = {} in\n{}", name, e0, e1),
        }
    }
}

pub struct Definition {
    f_name: Identifier,
    param_name: Identifier,
    f_body: Box<Ast>,
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fun {} {} =\n{}", self.f_name, self.param_name, self.f_body)
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
                    // bounded
                    Some(i) => Box::new(Expr::Var(i)),
                    // free
                    None => unimplemented!(),
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
            Ast::App(f, param) => {
                let e = param.de_bruijn_index_prim(names, depth);
                Box::new(Expr::App(f.clone(), e))
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
            
impl Definition {
    pub fn new<T: ToString, S: ToString, E: Into<Box<Ast>>>(t: T, s: S, e: E) -> Self {
        Definition {
            f_name: t.to_string(),
            param_name: s.to_string(),
            f_body: e.into(),
        }
    }

    pub fn de_bruijn_index(&self) -> Function {
        let names = Rc::new(List::new(&self.param_name[..], Rc::new(List::Empty)));
        let e = self.f_body.de_bruijn_index_prim(names, 1);
        Function::new(self.f_name.clone(), e)
    }
}

pub enum Item {
    Expression(Ast),
    FunDef(Definition),
}

