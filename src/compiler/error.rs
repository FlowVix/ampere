use crate::{
    error::ErrorReport,
    lexer::{error::LexerError, tokens::Token},
    source::CodeArea,
    special_fmt,
};

#[derive(Debug, Clone)]
pub enum CompilerError {
    NonexistentVariable(String, CodeArea),
    // InvalidBinaryOperands {
    //     a: (ValueType, CodeArea),
    //     b: (ValueType, CodeArea),
    //     op: &'static str,
    //     area: CodeArea,
    // },
    // NonBoolCondition {
    //     v: (ValueType, CodeArea),
    //     area: CodeArea,
    // },
    // TypeMismatch {
    //     v: (ValueType, CodeArea),
    //     expected: ValueType,
    //     area: CodeArea,
    // },
    // DestructureLenMismatch {
    //     expected: usize,
    //     found: usize,
    //     val_area: CodeArea,
    //     area: CodeArea,
    // },
}

impl CompilerError {
    pub fn into_report(self) -> ErrorReport {
        let (msg, labels) = match self {
            CompilerError::NonexistentVariable(name, area) => (
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
