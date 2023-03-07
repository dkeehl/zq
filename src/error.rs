use crate::lexer::LError;
use crate::parser::PError;
use crate::ir::TypeError;

#[derive(Debug)]
pub enum ZqError {
    Lexer(LError),
    Parser(PError),
    Type(TypeError),
}

impl From<LError> for ZqError {
    fn from(e: LError) -> Self {
        Self::Lexer(e)
    }
}

impl From<PError> for ZqError {
    fn from(e: PError) -> Self {
        Self::Parser(e)
    }
}

impl From<TypeError> for ZqError {
    fn from(e: TypeError) -> Self {
        Self::Type(e)
    }
}

