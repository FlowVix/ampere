use itertools::Itertools;
use lasso::Spur;

use crate::source::CodeSpan;

use super::operators::{AssignOp, BinOp, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub struct ExprNode {
    pub typ: ExprType,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Int(i64),
    Float(f64),
    String(Spur),
    Bool(bool),

    Var(Spur),

    Unary(UnaryOp, Box<ExprNode>),
    Op(Box<ExprNode>, BinOp, Box<ExprNode>),

    Index {
        base: Box<ExprNode>,
        index: Box<ExprNode>,
    },
    Member {
        base: Box<ExprNode>,
        member: Spur,
    },
    Associated {
        base: Box<ExprNode>,
        member: Spur,
    },
    Call {
        base: Box<ExprNode>,
        args: Vec<ExprNode>,
    },

    Array(Vec<ExprNode>),
    Tuple(Vec<ExprNode>),

    Block(Box<Stmts>),

    If {
        cond: Box<ExprNode>,
        then: Box<ExprNode>,
        otherwise: Option<Box<ExprNode>>,
    },
    While {
        cond: Box<ExprNode>,
        body: Box<ExprNode>,
    },
    Function {
        params: Vec<(LetPatternNode, Option<ExprNode>)>,
        ret_type: Option<Box<ExprNode>>,
        body: Box<ExprNode>,
    },

    Dbg(Box<ExprNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtNode {
    pub typ: StmtType,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    Expr(ExprNode),

    Let(LetPatternNode, ExprNode),
    Assign(AssignPatternNode, ExprNode),
    AssignOp(AssignPatternNode, AssignOp, ExprNode),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmts {
    Normal(Vec<StmtNode>),
    Ret(Vec<StmtNode>, StmtNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignPatternNode {
    pub typ: AssignPatternType,
    pub span: CodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignPatternType {
    Path { var: Spur, path: Vec<AssignPath> },

    ArrayDestructure(Vec<AssignPatternNode>),
    TupleDestructure(Vec<AssignPatternNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignPath {
    Index(ExprNode),
    Member(Spur),
    Associated(Spur),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetPatternNode {
    pub typ: LetPatternType,
    pub span: CodeSpan,
}
// impl LetPatternNode {
//     pub fn to_str(&self) -> String {
//         match &self.typ {
//             LetPatternType::Var(v) => v.clone(),
//             LetPatternType::ArrayDestructure(v) => {
//                 format!("[{}]", v.iter().map(|v| v.to_str()).join(", "))
//             }
//             LetPatternType::TupleDestructure(v) => {
//                 format!("[{}]", v.iter().map(|v| v.to_str()).join(", "))
//             }
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum LetPatternType {
    Var(Spur),

    ArrayDestructure(Vec<LetPatternNode>),
    TupleDestructure(Vec<LetPatternNode>),
}
