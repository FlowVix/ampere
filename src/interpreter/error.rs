use crate::{
    error::ErrorReport,
    lexer::{error::LexerError, tokens::Token},
    source::CodeArea,
    special_fmt,
};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    NonexistentVariable(String, CodeArea),
}

impl RuntimeError {
    pub fn into_report(self) -> ErrorReport {
        let (msg, labels) = match self {
            RuntimeError::NonexistentVariable(name, area) => (
                "Nonexistent variable",
                vec![(area, special_fmt!("Variable {} does not exist", name))],
            ),
        };

        ErrorReport {
            typ: "Runtime Error",
            msg,
            labels,
        }
    }
}
