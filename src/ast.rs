use crate::ir::{Expr, Type, Variable, Function,};
use crate::lexer::Operator;
use std::rc::Rc;
use std::fmt;

#[derive(Clone)]
pub enum Ast {
    Int(u64),
    Ident(Identifier),
    Lam(Identifier, Type, Rc<Ast>),
    App(Rc<Ast>, Rc<Ast>, Rc<Vec<Ast>>),
    Op2(Operator, Rc<Ast>, Rc<Ast>),
    Let(Rc<Ast>, Identifier, Rc<Ast>),
    Match(Identifier, Rc<Vec<(Pattern, Rc<Ast>)>>),
}

#[derive(Clone)]
pub enum Pattern {
    Var(Identifier),
    Int(u64),
    Or(Vec<u64>),
}

impl Pattern {
    fn binds(&self, name: &Identifier) -> bool {
        match self {
            Pattern::Var(x) => x == name,
            _ => false,
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Var(s) => write!(f, "{}", s),
            Pattern::Int(n) => write!(f, "{}", n),
            Pattern::Or(ps) => {
                if let &[p, ref tail @ ..] = &ps[..] {
                    write!(f, "{}", p);
                    for p in tail.iter() {
                        write!(f, " | {}", p);
                    }
                } else {
                    unreachable!()
                }
                Ok(())
            },
        }
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
            Ast::Match(var, arms) => {
                write!(f, "match {} with {{\n", var);
                for (pat, ast) in arms.iter() {
                    write!(f, "{} => {};\n", pat, ast);
                }
                write!(f, "}}")
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Src(Rc<String>),
    Gen(usize)
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Identifier::Src(s) => write!(f, "{}", s),
            Identifier::Gen(n) => write!(f, "%{}", n),
        }
    }
}

impl Identifier {
    fn is_placeholder(&self) -> bool {
        match self {
            Identifier::Src(s) if s.as_str() == "_" => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
enum List<T> {
    Empty,
    Cons {
        head: T,
        tail: Rc<List<T>>,
    },
}

impl<T: PartialEq> List<T> {
    fn new(head: T, tail: Rc<Self>) -> Self {
        List::Cons {
            head,
            tail,
        }
    }

    fn subst(&self, name: &T, depth: u64) -> Option<u64> {
        match self {
            List::Empty => None,
            List::Cons { head, tail } => {
                if head == name {
                    Some(depth)
                } else {
                    tail.subst(name, depth - 1)
                }
            },
        }
    }
}

impl Ast {
    pub fn lam<T: Into<Rc<Ast>>>(s: Identifier, ty: Type, e: T) -> Ast {
        Ast::Lam(s, ty, e.into())
    }

    pub fn app<T: Into<Rc<Ast>>, S: Into<Rc<Ast>>>(f: T, arg: S, args: Vec<Ast>) -> Ast {
        Ast::App(f.into(), arg.into(), Rc::new(args))
    }

    pub fn local<T: Into<Rc<Ast>>, S: Into<Rc<Ast>>>(x: Identifier, e0: T, e1: S) -> Ast {
        Ast::Let(e0.into(), x, e1.into())
    }

    pub fn op2<T: Into<Rc<Ast>>, S: Into<Rc<Ast>>>(op: Operator, l: T, r: S) -> Ast {
        Ast::Op2(op, l.into(), r.into())
    }

    fn subst(&self, find: &Identifier, to: &Identifier) -> Ast {
        if find == to || find.is_placeholder() || to.is_placeholder() {
            return self.clone();
        }
        match self {
            Ast::Ident(v) if v == find => Ast::Ident(to.clone()),
            Ast::Lam(x, t, a) if x != find => Ast::lam(x.clone(), t.clone(), a.subst(find, to)),
            Ast::App(f, arg, args) => {
                let f = f.subst(find, to);
                let arg = arg.subst(find, to);
                let args: Vec<Ast> = args.iter()
                    .map(|a| a.subst(find, to))
                    .collect();
                Ast::app(f, arg, args)
            },
            Ast::Op2(op, a0, a1) => {
                let a0 = a0.subst(find, to);
                let a1 = a1.subst(find, to);
                Ast::op2(*op, a0, a1)
            },
            Ast::Let(e0, x, e1) => {
                let e0 = e0.subst(find, to);
                let e1 = if x != find {
                    Rc::new(e1.subst(find, to))
                } else {
                    e1.clone()
                };
                Ast::local(x.clone(), e0, e1)
            },
            Ast::Match(x, arms) => {
                let x = if x == find {
                    to.clone()
                } else {
                    x.clone()
                };
                let arms = arms.iter()
                    .map(|&(ref p, ref a)| {
                        let a = if !p.binds(&find) {
                            Rc::new(a.subst(find, to))
                        } else {
                            a.clone()
                        };
                        (p.clone(), a)
                    })
                    .collect();
                Ast::Match(x, Rc::new(arms))
            },
            _ => self.clone(),
        }
    }

    fn de_bruijn_index_prim(&self, names: Rc<List<&Identifier>>, depth: u64) -> Box<Expr> {
        match self {
            Ast::Int(n) => Box::new(Expr::Const(*n)),
            Ast::Ident(name) => {
                match names.subst(&name, depth) {
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
                let names = Rc::new(List::new(x, names));
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
                let names = Rc::new(List::new(x, names));
                let e1 = a1.de_bruijn_index_prim(names, depth + 1);
                Box::new(Expr::Let(e0, e1))
            },
            Ast::Match(var, arms) => {
                let (tab, default, asts) = compile_pm(var, &arms[..]);
                let tab = tab.into_iter().rev().collect();
                let es = asts.into_iter()
                    .map(|a| a.de_bruijn_index_prim(names.clone(), depth))
                    .collect();
                let var = Ast::Ident(var.clone()).de_bruijn_index_prim(names, depth);
                Box::new(Expr::Match(var, tab, default, es))
            }
        }
    }

    pub fn de_bruijn_index(&self) -> Box<Expr> {
        let names = Rc::new(List::Empty);
        self.de_bruijn_index_prim(names, 0)
    }
}

fn compile_pm(var: &Identifier, branchs: &[(Pattern, Rc<Ast>)])
    -> (Vec<(u64, usize)>, Option<usize>, Vec<Rc<Ast>>)
{
    match branchs {
        [] => (vec![], None, vec![]),
        [(Pattern::Var(x), a), ..] => {
            let a = a.subst(x, var);
            (vec![], Some(0), vec![Rc::new(a)])
        },
        [(Pattern::Int(n), a), ref tail @ ..] => {
            let (mut tab, default, mut asts) = compile_pm(var, tail);
            tab.push((*n, asts.len()));
            asts.push(a.clone());
            (tab, default, asts)
        },
        [(Pattern::Or(ps), a), ref tail @ ..] => {
            let (mut tab, default, mut asts) = compile_pm(var, tail);
            for i in ps.iter().rev() {
                tab.push((*i, asts.len()));
            }
            asts.push(a.clone());
            (tab, default, asts)
        },
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
    pub fn new<T: Into<Identifier>, E: Into<Box<Ast>>>
        (t: T, arg: (Identifier, Type), args: Vec<(Identifier, Type)>, ret_type: Type, e: E)
        -> Self
    {
        Definition {
            f_name: t.into(),
            arg,
            args,
            ret_type,
            f_body: e.into(),
        }
    }

    pub fn de_bruijn_index(&self) -> Function {
        let mut names = Rc::new(List::new(&self.arg.0, Rc::new(List::Empty)));
        for (x, _) in self.args.iter() {
            names = Rc::new(List::new(x, names));
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

