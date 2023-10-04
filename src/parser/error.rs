use crate::{
    error::ErrorDisplay,
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

impl From<ParserError> for ErrorDisplay {
    fn from(value: ParserError) -> Self {
        const TYP: &str = "Parsing Error";

        let (msg, area) = match value {
            ParserError::LexingError { error, area } => {
                return ErrorDisplay {
                    typ: "Lexing Error",
                    msg: error.msg(),
                    area,
                }
            }
            ParserError::UnexpectedToken {
                expected,
                found,
                area,
            } => (
                format!("Expected {}, found {}", expected, found.to_str()),
                area,
            ),
        };

        ErrorDisplay {
            typ: TYP,
            msg,
            area,
        }
    }
}

// impl From<LexerError> for ParserError {
//     fn from(value: LexerError) -> Self {
//         Self::LexingError(value)
//     }
// }
