use crate::lexer::{Lexer, LToken, Token::{self,*}, Keyword, Operator, Location};
use crate::ast::{Ast, Definition, Item};
use std::cell::{Cell, RefCell};
use std::ops::Range;

type PResult<T> = Result<T, PError>;

pub struct PState {
}

impl PState {
    pub fn new() -> Self {
        PState {}
    }
}

#[derive(Debug)]
enum TokenDesc {
    Number,
    Identifier,
    Type,
    Kw(Keyword),
    AnyOp,
    Op(Operator),
    Sep,
    Whitespace,
}

impl<'a> From<&Token<'a>> for TokenDesc {
    fn from(t: &Token) -> Self {
        match t {
            Token::TkInt(_) => Self::Number,
            Token::TkIdent(_) => Self::Identifier,
            Token::TkCapitalized(_) => Self::Type,
            Token::TkKeyWord(kw) => Self::Kw(*kw),
            Token::TkOp(op) => Self::Op(*op),
            Token::TkSeparater(_) => Self::Sep,
            Token::TkWhitespace | Token::TkComment => Self::Whitespace,
        }
    }
}

impl TokenDesc {
    fn inspect(&self) -> &'static str {
        match self {
            Self::Number => "number",
            Self::Identifier => "identifier",
            Self::Type => "type",
            Self::Kw(w) => w.inspect(),
            Self::AnyOp => "any operator",
            Self::Op(o) => o.inspect(),
            Self::Sep => "seperater",
            Self::Whitespace => "whitespace",
        }
    }
}

#[derive(Debug)]
pub struct TokenSet {
    inner: TokenDesc,
}

impl TokenSet {
    fn new(desc: TokenDesc) -> Self {
        TokenSet {
            inner: desc
        }
    }
}

#[derive(Debug)]
pub enum PError {
    Unexpected{
        span: Range<usize>,
        expect: TokenSet,
    },
    Unclosed,
    Eof,
}

pub trait Parser<'a> {
    type Output;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Self::Output>;
}

trait Combine<'a>: Parser<'a> + Sized {

    fn map<B, F>(self, f: F) -> Map<Self, F>
        where F: Fn(Self::Output) -> B
    {
        Map {
            p: self,
            f,
        }
    }

    fn seq<P: Parser<'a>>(self, other: P) -> Seq<Self, P> {
        Seq {
            a: self,
            b: other,
        }
    }

    fn choice<P: Parser<'a>>(self, other: P) -> Choice<Self, P> {
        Choice {
            a: self,
            b: other,
        }
    }

}

impl<'a, T: Parser<'a> + Sized> Combine<'a> for T {}

struct BoxedParser<T> {
    inner: Box<dyn for<'a> Parser<'a, Output=T>>
}

fn boxed<T, P>(p: P) -> BoxedParser<T>
where P: for<'a> Parser<'a, Output=T> + 'static
{
    BoxedParser { inner: Box::new(p) }
}

macro_rules! seq {
    ($a:expr, $b:expr) => {
        Combine::seq($a, $b)
    };
    ($a:expr, $($tail:expr),+) => {
        Combine::seq($a, seq!($($tail),+))
    };
}

macro_rules! choice {
    ($a:expr, $b:expr) => {
        Combine::choice($a, $b)
    };
    ($a:expr, $($tail:expr),+) => {
        Combine::choice($a, choice!($($tail),+))
    };
}

impl<'a, T, F> Parser<'a> for F
where F: Fn(&mut Lexer<'a>, &mut PState) -> PResult<T>
{
    type Output = T;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<T> {
        (self)(input, state)
    }
}

impl<'a, T> Parser<'a> for BoxedParser<T> {
    type Output = T;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<T> {
        self.inner.parse(input, state)
    }
}

struct Delay<T> {
    cache: RefCell<Option<T>>,
    f: Cell<Option<fn() -> T>>
}

fn delay<T>(f: fn() -> T) -> Delay<T> {
    Delay {
        cache: RefCell::new(None),
        f: Cell::new(Some(f)),
    }
}

impl<'a, P> Parser<'a> for Delay<P>
where P: Parser<'a>
{
    type Output=P::Output;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Self::Output> {
        if self.cache.borrow().is_some() {
            self.cache.borrow().as_ref()
                .unwrap()
                .parse(input, state)
        } else {
            let parser = (self.f.take().unwrap())();
            let ret = parser.parse(input, state);
            self.cache.replace(Some(parser));
            ret
        }
    }
}

//
// Combinators
//
struct Map<P, F> {
    p: P,
    f: F,
}

impl<'a, B, P, F> Parser<'a> for Map<P, F>
where P: Parser<'a>,
      F: Fn(P::Output) -> B
{
    type Output=B;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<B> {
        let res = self.p.parse(input, state)?;
        Ok((self.f)(res))
    }
}

struct Seq<A, B> {
    a: A,
    b: B,
}

impl<'a, A, B> Parser<'a> for Seq<A, B>
where A: Parser<'a>,
      B: Parser<'a>
{
    type Output = (A::Output, B::Output);

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Self::Output> {
        let save = input.checkpoint();
        let l = self.a.parse(input, state)?;
        match self.b.parse(input, state) {
            Ok(r) => Ok((l, r)),
            Err(e) => {
                input.reset(save);
                Err(e)
            },
        }
    }
}

struct Choice<A, B> {
    a: A,
    b: B,
}

impl<'a, A, B, T> Parser<'a> for Choice<A, B>
where A: Parser<'a, Output=T>,
      B: Parser<'a, Output=T>
{
    type Output = T;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<T> {
        match self.a.parse(input, state) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.b.parse(input, state)
            },
        }
    }
}

//
// atomic
//
macro_rules! token {
    ($input:ident, $var:ident with $tk:pat_param $(if $guard:expr)* => $f:expr, $expect:expr) => {
        if let Some($var) = $input.peek() {
            match $var.tok {
                $tk $(if $guard)* => {
                    let res = $f;
                    $input.consume();
                    Ok(res)
                },
                _ => {
                    let span = $var.loc.span.clone();
                    let expect = TokenSet::new($expect);
                    let e = PError::Unexpected { span, expect };
                    Err(e)
                },
            }
        } else {
            Err(PError::Eof)
        }
    };
    ($input:ident, $tk:pat_param => $f:expr, $expect:expr) => {
        token!($input, tk with $tk => $f, $expect)
    }
}

fn integer(input: &mut Lexer, state: &mut PState) -> PResult<Ast> {
    token!(input, Token::TkInt(n) => Ast::Int(n), TokenDesc::Number)
}

fn identifier<'a>(input: &mut Lexer<'a>, state: &mut PState) -> PResult<&'a str> {
    token!(input, Token::TkIdent(name) => name, TokenDesc::Identifier)
}

fn p_identifier<'a>(input: &mut Lexer<'a>, state: &mut PState) -> PResult<Ast> {
    token!(input, Token::TkIdent(name) => Ast::Ident(name.to_string()), TokenDesc::Identifier)
}

fn op(input: &mut Lexer, state: &mut PState) -> PResult<Operator> {
    token!(input, Token::TkOp(o) => o, TokenDesc::AnyOp)
}

struct Tk(Token<'static>);

fn token(tk: Token<'static>) -> Tk {
    Tk(tk)
}

impl<'a> Parser<'a> for Tk {
    type Output = Location;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Location> {
        token!(input, tk with ref t if t == &self.0 => tk.loc.clone(), (&self.0).into())
    }
}

// Pratt parser for operator precedance 
fn pratt<'a, P>(operand: &P, input: &mut Lexer<'a>, state: &mut PState, power: u8)
    -> PResult<(Ast, Option<(Operator, u8, u8)>)>
    where P: Parser<'a, Output=Ast>
{
    let mut acc = operand.parse(input, state)?;
    // FIXME parse only operators that do binary operations
    let mut infix = match op(input, state) {
        Ok(o) => o,
        Err(PError::Eof) | Err(PError::Unexpected {..}) => return Ok((acc, None)),
        Err(_) => unreachable!(),
    };
    let (mut l_pow, mut r_pow) = infix.assosiate_power();

    while l_pow > power {
        let (rhs, next_infix) = pratt(operand, input, state, r_pow)?;
        acc = Ast::Op2(infix, Box::new(acc), Box::new(rhs));
        if let Some((next_infix, next_l_pow, next_r_pow)) = next_infix {
            infix = next_infix;
            l_pow = next_l_pow;
            r_pow = next_r_pow;
        } else {
            return Ok((acc, None));
        }
    }
    assert_ne!(l_pow, power);
    Ok((acc, Some((infix, l_pow, r_pow))))
}

struct OpExpr<T> {
    operand: T,
}

impl<'a, T> Parser<'a> for OpExpr<T>
where T: Parser<'a, Output=Ast>
{
    type Output = Ast;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Ast> {
        let save = input.checkpoint();
        match pratt(&self.operand, input, state, 0) {
            Ok((x, None)) => Ok(x),
            // `pratt` returns with an empty rhs only when the left power of the op is less than the power,
            // which is 0_u8 here, and thus impossible.
            Ok(_) => unreachable!(),
            Err(e) => {
                input.reset(save);
                Err(e)
            }
        }
    }
}

//
// Sytax
//
fn expression() -> impl for<'a> Parser<'a, Output=Ast> {
    boxed(choice!(p_let(), p_operator_expression()))
}

fn p_atomic() -> impl for<'a> Parser<'a, Output=Ast> {
    let pe = seq!(token(TkSeparater('(')), delay(expression), token(TkSeparater(')')))
        .map(|(_, (e, _))| e);
    choice!(integer, p_identifier, pe)
}

fn p_let() -> impl for<'a> Parser<'a, Output=Ast> {
    let f = |(_, (id, (_, (e0, (_, e1))))): (_, (&str, (_, (_, (_, _)))))| {
        let e0 = Box::new(e0);
        let e1 = Box::new(e1);
        Ast::Let(e0, id.to_string(), e1)
    };
    seq!(token(TkKeyWord(Keyword::Let)),
        identifier,
        token(TkOp(Operator::Equal)),
        delay(expression),
        token(TkKeyWord(Keyword::In)),
        delay(expression))
        .map(f)
}

fn p_application() -> impl for<'a> Parser<'a, Output=Ast> {
    let f = |(f_name, param): (&str, _)| {
        let param = Box::new(param);
        Ast::App(f_name.to_string(), param)
    };
    seq!(identifier, p_atomic()).map(f)
}

fn p_operand() -> impl for<'a> Parser<'a, Output=Ast> {
    choice!(p_application(), p_atomic())
}

fn p_operator_expression() -> impl for<'a> Parser<'a, Output=Ast> {
    OpExpr {
        operand: p_operand(),
    }
}

fn function_def() -> impl for<'a> Parser<'a, Output=Definition> {
    let f = |(_, (f_name, (p_name, (_, e)))): (_, (&str, (&str, (_, _))))| {
        Definition::new(f_name, p_name, e)
    };
    seq!(token(TkKeyWord(Keyword::Fun)),
        identifier,
        identifier,
        token(TkOp(Operator::Equal)),
        expression())
        .map(f)
}

pub fn item() -> impl for<'a> Parser<'a, Output=Item> {
    let expr = expression().map(Item::Expression);
    let fun_def = function_def().map(Item::FunDef);
    choice!(fun_def, expr)
}

