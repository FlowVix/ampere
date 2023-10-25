use crate::{
    error::ErrorReport,
    lexer::{error::LexerError, tokens::Token},
    parser::error::ParserError,
    source::CodeArea,
    special_fmt,
};

#[derive(Debug, Clone)]
pub enum CompilerError {
    NonexistentVariable(String, CodeArea),
    BreakOutsideLoop(CodeArea),
    ContinueOutsideLoop(CodeArea),
    ImportParseError(CodeArea, ParserError),
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
            CompilerError::BreakOutsideLoop(area) => (
                "Break used outside of loop",
                vec![(area, "This break was used outside of a loop".into())],
            ),
            CompilerError::ContinueOutsideLoop(area) => (
                "Continue used outside of loop",
                vec![(area, "This continue was used outside of a loop".into())],
            ),
            CompilerError::ImportParseError(area, err) => {
                let mut labels = vec![(area.clone(), "Import occured here".into())];

                labels.extend(err.into_report().labels);

                ("Error parsing import", labels)
            }
        };

        ErrorReport {
            typ: "Runtime Error",
            msg,
            labels,
        }
    }
}
