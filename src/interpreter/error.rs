use crate::{
    error::ErrorReport,
    lexer::{error::LexerError, tokens::Token},
    source::CodeArea,
    special_fmt,
};

use super::value::ValueType;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    NonexistentVariable(String, CodeArea),
    InvalidBinaryOperands {
        a: (ValueType, CodeArea),
        b: (ValueType, CodeArea),
        op: &'static str,
        area: CodeArea,
    },
}

impl RuntimeError {
    pub fn into_report(self) -> ErrorReport {
        let (msg, labels) = match self {
            RuntimeError::NonexistentVariable(name, area) => (
                "Nonexistent variable",
                vec![(area, special_fmt!("Variable {} does not exist", name))],
            ),
            RuntimeError::InvalidBinaryOperands { a, b, op, area } => (
                "Invalid operands",
                vec![
                    (
                        area,
                        special_fmt!(
                            "Operator `{}` cannot be used on {} and {}",
                            op,
                            a.0.name(),
                            b.0.name(),
                        ),
                    ),
                    (a.1, special_fmt!("This is of type `{}`", a.0.name())),
                    (b.1, special_fmt!("This is of type `{}`", b.0.name())),
                ],
            ),
        };

        ErrorReport {
            typ: "Runtime Error",
            msg,
            labels,
        }
    }
}
