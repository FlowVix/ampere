use crate::{
    error::ErrorReport,
    lexer::{error::LexerError, tokens::Token},
    source::CodeArea,
    special_fmt,
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

impl ParserError {
    pub fn into_report(self) -> ErrorReport {
        let (msg, labels) = match self {
            ParserError::LexingError { error, area } => {
                ("Error during lexing", vec![(area, error.msg())])
            }
            ParserError::UnexpectedToken {
                expected,
                found,
                area,
            } => (
                "Unexpected token",
                vec![(
                    area,
                    special_fmt!("Expected `{}`, found `{}`", expected, found.to_str()),
                )],
            ),
        };

        ErrorReport {
            typ: "Parser Error",
            msg,
            labels,
        }
    }
}
