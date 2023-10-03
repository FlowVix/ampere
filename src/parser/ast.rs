use crate::source::CodeSpan;

use super::operators::{BinOp, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub struct ExprNode {
    pub typ: ExprType,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Var(String),

    Unary(UnaryOp, Box<ExprNode>),
    Op(Box<ExprNode>, BinOp, Box<ExprNode>),
}
