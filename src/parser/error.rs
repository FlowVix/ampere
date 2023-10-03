use crate::{
    lexer::{error::LexerError, tokens::Token},
    source::CodeArea,
};

#[derive(Debug, Clone)]
pub enum ParserError {
    LexingError {
        error: LexerError,
        area: CodeArea,
    },
    UnexpectedToken {
        expected: &'static str,
        found: Token,
        area: CodeArea,
    },
}

// impl From<LexerError> for ParserError {
//     fn from(value: LexerError) -> Self {
//         Self::LexingError(value)
//     }
// }
