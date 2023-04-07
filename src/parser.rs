use crate::lexer::{Lexer, LToken, Token::{self,*}, Keyword, Operator, Delimiter, Location};
use crate::ast::{Ast, Definition, Item, Pattern, Identifier};
use crate::ir::Type;
use std::cell::{Cell, RefCell};
use std::ops::Range;
use std::rc::Rc;

type PResult<T> = Result<T, PError>;

pub struct PState {
    counter: usize,
}

impl PState {
    pub fn new() -> Self {
        PState {
            counter: 0,
        }
    }

    fn fresh_name(&mut self) -> usize {
        let res = self.counter;
        self.counter += 1;
        res
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
    Delim(Delimiter),
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
            Token::TkDelim(sym) => Self::Delim(*sym),
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
            Self::Delim(sym) => sym.inspect(),
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

struct BoxedParser<T> {
    inner: Rc<dyn for<'a> Parser<'a, Output=T>>
}

fn boxed<T, P>(p: P) -> BoxedParser<T>
where P: for<'a> Parser<'a, Output=T> + 'static
{
    BoxedParser { inner: Rc::new(p) }
}

impl<T> Clone for BoxedParser<T> {
    fn clone(&self) -> Self {
        BoxedParser {
            inner: self.inner.clone(),
        }
    }
}

impl<'a, T> Parser<'a> for BoxedParser<T> {
    type Output = T;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<T> {
        self.inner.parse(input, state)
    }
}

#[derive(Clone)]
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
#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
struct ZeroOrMore<P> {
    p: P
}

impl<'a, P> Parser<'a> for ZeroOrMore<P>
where P : Parser<'a>
{
    type Output = Vec<P::Output>;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<Self::Output> {
        let mut ret = vec![];
        while let Ok(v) = self.p.parse(input, state) {
            ret.push(v);
        }
        Ok(ret)
    }
}

fn zero_or_more<P>(p: P) -> ZeroOrMore<P> {
    ZeroOrMore { p }
}

fn success(input: &mut Lexer, state: &mut PState) -> PResult<()> {
    Ok(())
}

fn fresh_name(input: &mut Lexer, state: &mut PState) -> PResult<usize> {
    Ok(state.fresh_name())
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

fn integer(input: &mut Lexer, state: &mut PState) -> PResult<u64> {
    token!(input, Token::TkInt(n) => n, TokenDesc::Number)
}

fn identifier<'a>(input: &mut Lexer<'a>, state: &mut PState) -> PResult<&'a str> {
    token!(input, Token::TkIdent(name) => name, TokenDesc::Identifier)
}

fn op(input: &mut Lexer, state: &mut PState) -> PResult<Operator> {
    token!(input, Token::TkOp(o) => o, TokenDesc::AnyOp)
}

#[derive(Clone)]
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
fn pratt<'a, P, T, F>(operand: &P, input: &mut Lexer<'a>, state: &mut PState, power: u8, f: &F)
    -> PResult<(T, Option<(Operator, u8, u8)>)>
    where P: Parser<'a, Output=T>,
          F: Fn(Operator, T, T) -> T,
{
    let mut acc = operand.parse(input, state)?;
    let mut infix = match op(input, state) {
        Ok(o) => o,
        Err(PError::Eof) | Err(PError::Unexpected {..}) => return Ok((acc, None)),
        Err(_) => unreachable!(),
    };
    let (mut l_pow, mut r_pow) = infix.assosiate_power();

    while l_pow > power {
        let (rhs, next_infix) = pratt(operand, input, state, r_pow, f)?;
        acc = f(infix, acc, rhs);
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

struct OpExpr<P, F> {
    operand: P,
    f: F,
}

impl<'a, P, T, F> Parser<'a> for OpExpr<P, F>
where P: Parser<'a, Output=T>,
      F: Fn(Operator, T, T) -> T,
{
    type Output = T;

    fn parse(&self, input: &mut Lexer<'a>, state: &mut PState) -> PResult<T> {
        let save = input.checkpoint();
        match pratt(&self.operand, input, state, 0, &self.f) {
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

fn parenthesised<P, T>(p: P) -> impl for<'a> Parser<'a, Output=T> + Clone
where P: for<'a> Parser<'a, Output=T> + Clone
{
    seq!(token(TkDelim(Delimiter::ParL)), p, token(TkDelim(Delimiter::ParR)))
        .map(|(_, (t, _))| t)
}

fn expression() -> impl for<'a> Parser<'a, Output=Ast> + Clone {
    boxed(choice!(p_let(), p_match(), lambda(), p_operator_expression()))
}

fn p_atomic() -> impl for<'a> Parser<'a, Output=Ast> + Clone {
    let in_par = parenthesised(delay(expression));
    choice!(p_integer(), p_identifier(), in_par)
}

fn p_integer() -> impl for<'a> Parser<'a, Output=Ast> + Clone {
    (integer).map(Ast::Int)
}

fn p_identifier() -> impl for<'a> Parser<'a, Output=Ast> + Clone {
    ident().map(Ast::Ident)
}

fn ident() -> impl for<'a> Parser<'a, Output=Identifier> + Clone {
    let f = |s: &str| Identifier::Src(Rc::new(s.to_string()));
    (identifier).map(f)
}

fn ty() -> impl for<'a> Parser<'a, Output=Type> + Clone {
    let tail = seq!(token(TkDelim(Delimiter::Arrow)), delay(ty))
        .map(|(_, t)| Some(t));
    let tail = choice!(tail, (success).map(|_| None));
    boxed(seq!(atomic_type(), tail).map(|(l, r)|
        if let Some(r) = r {
            Type::arrow(l, r)
        } else {
            l
        }))
}

fn atomic_type() -> impl for<'a> Parser<'a, Output=Type> {
    let nat = token(TkCapitalized("Nat")).map(|_| Type::Nat);
    let in_par = parenthesised(delay(ty));
    choice!(nat, in_par)
}

fn p_let() -> impl for<'a> Parser<'a, Output=Ast> {
    seq!(token(TkKeyWord(Keyword::Let)),
        ident(),
        token(TkDelim(Delimiter::Equal)),
        delay(expression),
        token(TkKeyWord(Keyword::In)),
        delay(expression))
        .map(|(_, (name, (_, (e0, (_, e1)))))| Ast::local(name, e0, e1))
}

fn p_application() -> impl for<'a> Parser<'a, Output=Ast> {
    seq!(p_atomic(), p_atomic(), zero_or_more(p_atomic()))
        .map(|(f, (arg, args))| Ast::app(f, arg, args))
}

fn p_operand() -> impl for<'a> Parser<'a, Output=Ast> {
    choice!(p_application(), p_atomic())
}

fn p_operator_expression() -> impl for<'a> Parser<'a, Output=Ast> {
    OpExpr {
        operand: p_operand(),
        f: Ast::op2,
    }
}

fn type_annotation<P, T>(p: P) -> impl for<'a> Parser<'a, Output=(T, Type)> + Clone
where P: for<'a> Parser<'a, Output=T> + Clone
{
    seq!(p, token(TkDelim(Delimiter::Colon)), ty()).map(|(x, (_, t))| (x, t))
}

fn lambda() -> impl for<'a> Parser<'a, Output=Ast> {
    seq!(token(TkDelim(Delimiter::BackSlash)),
        type_annotation(ident()),
        token(TkDelim(Delimiter::FatArrow)),
        delay(expression))
        .map(|(_, ((x, t), (_, e)))| Ast::lam(x, t, e))
}

fn pattern() -> impl for<'a> Parser<'a, Output=Pattern> {
    let int = seq!(integer, zero_or_more(seq!(token(TkOp(Operator::Or)), integer)))
        .map(|(i, v)| if v.is_empty() {
            Pattern::Int(i)
        } else {
            let mut ps = vec![i];
            v.into_iter().for_each(|(_, p)| ps.push(p));
            Pattern::Or(ps)
        });
    let var = ident().map(Pattern::Var);
    choice!(int, var)
}

fn arm() -> impl for<'a> Parser<'a, Output=(Pattern, Rc<Ast>)> {
    seq!(pattern(), token(TkDelim(Delimiter::FatArrow)), delay(expression))
        .map(|(p, (_, a))| (p, Rc::new(a)))
}

fn in_brace<P, T>(p: P) -> impl for<'a> Parser<'a, Output=T>
where P: for<'a> Parser<'a, Output=T>
{
    seq!(token(TkDelim(Delimiter::BraceL)), p, token(TkDelim(Delimiter::BraceR)))
        .map(|(_, (x, _))| x)
}

fn match_block() -> impl for<'a> Parser<'a, Output=Vec<(Pattern, Rc<Ast>)>> {
    let arms = zero_or_more(
        seq!(arm(), token(TkDelim(Delimiter::SemiColon))).map(|(x, _)| x));
    in_brace(arms)
}

fn p_match() -> impl for<'a> Parser<'a, Output=Ast> {
    seq!(token(TkKeyWord(Keyword::Match)),
        delay(expression),
        token(TkKeyWord(Keyword::With)),
        match_block(),
        fresh_name)
        .map(|(_, (e, (_, (arms, x))))| match e {
            Ast::Ident(var) => Ast::Match(var, Rc::new(arms)),
            _ => {
                let x = Identifier::Gen(x);
                Ast::local(x.clone(), e, Ast::Match(x, Rc::new(arms)))
            }
        })
}

fn function_def() -> impl for<'a> Parser<'a, Output=Definition> {
    let arg = type_annotation(ident());
    let arg1 = seq!(token(TkDelim(Delimiter::Comma)), arg.clone()).map(|(_, x)| x);
    seq!(token(TkKeyWord(Keyword::Fun)),
        type_annotation(seq!(ident(), parenthesised(seq!(arg, zero_or_more(arg1))))),
        token(TkDelim(Delimiter::Equal)),
        expression())
        .map(|(_, (((f_name, (arg, args)), ret_type), (_, e)))|
                Definition::new(f_name, arg, args, ret_type, e))
}

pub fn item() -> impl for<'a> Parser<'a, Output=Item> {
    let expr = expression().map(Item::Expression);
    let fun_def = function_def().map(Item::FunDef);
    choice!(fun_def, expr)
}

