use std::ops::Range;
use std::str::Chars;
use std::fmt;

#[derive(Clone)]
pub struct LToken<'a> {
    pub tok: Token<'a>,
    pub loc: Location,
}

impl<'a> fmt::Display for LToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at row {} column {}", self.tok, self.loc.row, self.loc.col)
    }
}

#[derive(Clone)]
pub struct Location {
    pub span: Range<usize>,
    pub row: usize,
    pub col: usize,
}

impl Location {
    fn new(span: Range<usize>, row: usize, col: usize) -> Self {
        Location { span, row, col }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    TkInt(u64),
    TkIdent(&'a str),
    TkCapitalized(&'a str),
    TkKeyWord(Keyword),
    TkWhitespace,
    TkComment,
    TkOp(Operator),
    TkSeparater(char),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::TkInt(n) => write!(f, "Integer {}", n),
            Token::TkIdent(name) => write!(f, "Identifier {}", name),
            Token::TkCapitalized(name) => write!(f, "Type {}", name),
            Token::TkKeyWord(w) => write!(f, "Keyword {}", w),
            Token::TkWhitespace => write!(f, "Whitespace"),
            Token::TkComment => write!(f, "Comment"),
            Token::TkOp(op) => write!(f, "Operator {}", op),
            Token::TkSeparater(sym) => write!(f, "Seperater {}", sym),
        }
    }
}

impl<'a> LToken<'a> {
    pub fn is_white_space(&self) -> bool {
        match self.tok {
            Token::TkWhitespace | Token::TkComment => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

impl Operator {
    pub fn assosiate_power(&self) -> (u8, u8) {
        use Operator::*;
        match self {
            Plus | Minus => (40, 41),    
            Star | Slash => (50, 51),   
            Equal => (31, 30),   
        }
    }

    pub fn inspect(&self) -> &'static str {
        match self {
            Operator::Plus  => "+",
            Operator::Minus => "-",
            Operator::Star  => "*",
            Operator::Slash => "/",
            Operator::Equal => "=",
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Keyword {
    Let,
    In,
    Fun,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

impl Keyword {
    pub fn inspect(&self) -> &'static str {
        match self {
            Keyword::Let =>  "let",
            Keyword::In => "in",
            Keyword::Fun =>  "fun",
        }
    }
}

#[derive(Debug)]
pub enum LError {
    UnrecognizedToken(usize),
}

#[derive(Clone, Copy, Debug)]
enum LState {
    Start,
    StInteger,
    StIdent,
    StCapitalized,
    StKeyword(Keyword),
    StWhiteSpace,
    StComment,
    StSlash,
}

// pub type Lines<'a> = Vec<&'a str>;

struct Scanner<'a> {
    tokens: Vec<LToken<'a>>,

    text: &'a str,
    chars: Chars<'a>,

    state: LState,
    offset: usize,
    row: usize,
    col: usize,

    token_start: usize,
    token_row: usize,
    token_col: usize,
}

impl<'a> Scanner<'a> {
    fn new(text: &'a str) -> Self {
        Scanner {
            tokens: vec![],
            text, 
            chars: text.chars(),
            state: LState::Start,
            offset: 0,
            row: 1,
            col: 1,
            token_start: 0,
            token_row: 1,
            token_col: 1,
        }
    }

    fn scan(&mut self) -> Result<(), LError> {
        use LState::*;

        while let Some(c) = self.chars.next() {
            match c {
                '0'..='9' => match self.state {
                    Start => self.new_token(StInteger),
                    StKeyword(_) => self.state = StIdent,
                    StSlash | StWhiteSpace => {
                        self.commit();
                        self.new_token(StInteger);
                    },
                    _ => {},
                },

                '_' => match self.state {
                    Start => self.new_token(StIdent),
                    StKeyword(_) => self.state = StIdent,
                    StWhiteSpace | StSlash => {
                        self.commit();
                        self.new_token(StIdent);
                    },
                    _ => {},
                },

                c @ 'a'..='z' => match self.state {
                    Start => self.keyword_or_identifier(c),
                    StKeyword(_) => self.state = StIdent,
                    StInteger => return self.unrecognized(),
                    StWhiteSpace | StSlash => {
                        self.commit();
                        self.keyword_or_identifier(c)
                    },
                    _ => {},
                },
                    
                'A'..='Z' => match self.state {
                    Start => self.new_token(StCapitalized),
                    StInteger => return self.unrecognized(),
                    StKeyword(_) => self.state = StIdent,
                    StWhiteSpace | StSlash => {
                        self.commit();
                        self.new_token(StCapitalized);
                    },
                    _ => {},
                },
                
                '+' => self.one_char_token(Token::TkOp(Operator::Plus)),
                '-' => self.one_char_token(Token::TkOp(Operator::Minus)),
                '*' => self.one_char_token(Token::TkOp(Operator::Star)),
                '=' => self.one_char_token(Token::TkOp(Operator::Equal)),

                c @ ('{' | '}' | '(' | ')' | '[' | ']') =>
                    self.one_char_token(Token::TkSeparater(c)),

                '/' => match self.state {
                    Start => self.new_token(StSlash),
                    StComment => {},
                    StSlash => self.state = StComment,
                    _ => {
                        self.commit();
                        self.new_token(StSlash);
                    },
                },
                
                '\n' => {
                    self.commit();
                    self.state = Start;
                    self.row += 1;
                    self.col = 0;
                },

                c if c.is_ascii_whitespace() => match self.state {
                    Start => self.new_token(StWhiteSpace),
                    StWhiteSpace | StComment => {},
                    _ => {
                        self.commit();
                        self.new_token(StWhiteSpace);
                    }
                },

                _ => return self.unrecognized(),
            }
            self.offset += 1;
            self.col += 1;
        }
        self.commit();
        Ok(())
    }

    fn new_token(&mut self, s: LState) {
        self.state = s;
        self.token_start = self.offset;
        self.token_row = self.row;
        self.token_col = self.col;
    }

    fn one_char_token(&mut self, tok: Token<'a>) {
        match self.state {
            LState::StComment => {},
            _ => {
                self.commit();
                let i = self.offset;
                let loc = Location::new((i..i+1), self.row, self.col);
                let token = LToken { tok, loc };
                self.tokens.push(token);
                self.state = LState::Start;
            },
        }
    }

    fn commit(&mut self) {
        use LState::*;
        match self.state {
            Start | StWhiteSpace | StComment => return,
            st => {
                let (s, loc) = self.take_slice();
                let tok = match st {
                    StInteger => {
                        let i = s.parse::<u64>().unwrap();
                        Token::TkInt(i)
                    },
                    StIdent => Token::TkIdent(s),
                    
                    StCapitalized => Token::TkCapitalized(s),

                    StKeyword(w) => Token::TkKeyWord(w),

                    StSlash => Token::TkOp(Operator::Slash),

                    _ => unreachable!(),
                };
                let token = LToken { tok, loc };
                self.tokens.push(token);
            },
        }
    }

    fn token_location(&self) -> Location {
        let span = (self.token_start..self.offset);
        let row = self.token_row;
        let col = self.token_col;
        Location { span, row, col }
    }

    fn take_slice(&self) -> (&'a str, Location) {
        let loc = self.token_location();
        let s = unsafe { self.text.get_unchecked(loc.span.clone()) };
        (s, loc)
    }
        
    fn unrecognized(&self) -> Result<(), LError> {
        Err(LError::UnrecognizedToken(self.offset))
    }

    fn keyword_or_identifier(&mut self, c: char) {
        use LState::*;

        let s = self.chars.clone().as_str();
        let (state, count) = match c {
            'l' => if s.starts_with("et") {
                (StKeyword(Keyword::Let), 2)
            } else {
                (StIdent, 0)
            },
            'i' => if s.starts_with("n") {
                (StKeyword(Keyword::In), 1)
            } else {
                (StIdent, 0)
            },
            'f' => if s.starts_with("un") {
                (StKeyword(Keyword::Fun), 2)
            } else {
                (StIdent, 0)
            },
            _ => (StIdent, 0),
        };
        self.new_token(state);
        // Jump forward
        self.chars = s[count..].chars();
        self.offset += count;
        self.col += count;
    }
}

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    offset: usize,
    scaned: bool,
}

pub type CheckPoint = usize;

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        let scanner = Scanner::new(text);
        Lexer {
            scanner,
            offset: 0,
            scaned: false,
        }
    }

    pub fn scan(&mut self) -> Result<(), LError> {
        if !self.scaned {
            self.scanner.scan()?;
            self.scaned = true;
        }
        Ok(())
    }

    pub fn peek(&self) -> Option<&LToken<'a>> {
        if self.offset < self.scanner.tokens.len() {
            Some(&self.scanner.tokens[self.offset])
        } else {
            None
        }
    }

    pub fn consume(&mut self) {
        self.offset += 1;
    }

    pub fn checkpoint(&self) -> CheckPoint {
        self.offset
    }

    pub fn reset(&mut self, checkpoint: CheckPoint) {
        self.offset = checkpoint;
    }

    pub fn iter(&self) -> impl Iterator<Item=&LToken<'a>> {
        self.scanner.tokens[self.offset..].iter()
    }

    pub fn is_empty(&self) -> bool {
        self.offset == self.scanner.tokens.len()
    }
}

