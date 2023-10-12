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

    Index {
        base: Box<ExprNode>,
        index: Box<ExprNode>,
    },
    Member {
        base: Box<ExprNode>,
        member: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtNode {
    pub typ: StmtType,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    Expr(ExprNode),
    Let(String, ExprNode),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmts {
    Normal(Vec<StmtNode>),
    Ret(Vec<StmtNode>, StmtNode),
}
