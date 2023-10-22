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
    NonBoolCondition {
        v: (ValueType, CodeArea),
        area: CodeArea,
    },
    TypeMismatch {
        v: (ValueType, CodeArea),
        expected: ValueType,
        area: CodeArea,
    },
    DestructureLenMismatch {
        expected: usize,
        found: usize,
        val_area: CodeArea,
        area: CodeArea,
    },
    CannotCall {
        v: (ValueType, CodeArea),
        area: CodeArea,
    },
    IncorrectArgAmount {
        expected: usize,
        found: usize,
        val_area: CodeArea,
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
                    (a.1, special_fmt!("This is of type {}", a.0.name())),
                    (b.1, special_fmt!("This is of type {}", b.0.name())),
                ],
            ),
            RuntimeError::NonBoolCondition { v, area } => (
                "Invalid type for condition",
                vec![
                    (
                        area,
                        special_fmt!("{} cannot be used as a condition", v.0.name(),),
                    ),
                    (v.1, special_fmt!("This is of type {}", v.0.name())),
                ],
            ),
            RuntimeError::TypeMismatch { v, expected, area } => (
                "Type mismatch",
                vec![
                    (
                        area,
                        special_fmt!("Expected {}, found {}", expected.name(), v.0.name()),
                    ),
                    (v.1, special_fmt!("This is of type {}", v.0.name())),
                ],
            ),
            RuntimeError::DestructureLenMismatch {
                expected,
                found,
                val_area,
                area,
            } => (
                "Incorrect element amount for destructure",
                vec![
                    (
                        area,
                        special_fmt!("Expected {} elements, found {}", expected, found),
                    ),
                    (val_area, special_fmt!("This has {} elements", found)),
                ],
            ),
            RuntimeError::CannotCall { v, area } => (
                "Cannot call",
                vec![
                    (area, special_fmt!("Cannot call {}", v.0.name())),
                    (v.1, special_fmt!("This is of type {}", v.0.name())),
                ],
            ),
            RuntimeError::IncorrectArgAmount {
                expected,
                found,
                val_area,
                area,
            } => (
                "Incorrect amount of arguments",
                vec![
                    (
                        area,
                        special_fmt!("Expected {} arguments, found {}", expected, found),
                    ),
                    (
                        val_area,
                        special_fmt!("Function defined to take {} arguments here", found),
                    ),
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
