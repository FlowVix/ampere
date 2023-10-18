use std::rc::Rc;

use crate::{
    parser::ast::{ExprNode, ExprType},
    source::AmpereSource,
};

use self::{builder::CodeBuilder, bytecode::Constant, error::CompilerError, opcodes::Opcode};

pub mod builder;
pub mod bytecode;
pub mod error;
pub mod opcodes;
pub mod proto;
pub mod scope;

pub struct Compiler<'a> {
    src: &'a Rc<AmpereSource>,
    // scopes:
}

pub type CompileResult<T> = Result<T, CompilerError>;

impl<'a> Compiler<'a> {
    pub fn compile_expr(
        &mut self,
        expr: &ExprNode,
        builder: &mut CodeBuilder,
    ) -> CompileResult<()> {
        match &expr.typ {
            ExprType::Int(v) => builder.load_const(Constant::Int(*v), expr.span),
            ExprType::Float(v) => builder.load_const(Constant::Float(*v), expr.span),
            ExprType::String(v) => {
                builder.load_const(Constant::String(v.clone().into_boxed_str()), expr.span)
            }
            ExprType::Bool(v) => builder.load_const(Constant::Bool(*v), expr.span),
            ExprType::Var(_) => todo!(),
            ExprType::Unary(_, _) => todo!(),
            ExprType::Op(_, _, _) => todo!(),
            ExprType::Index { base, index } => todo!(),
            ExprType::Member { base, member } => todo!(),
            ExprType::Associated { base, member } => todo!(),
            ExprType::Call { base, args } => todo!(),
            ExprType::Array(_) => todo!(),
            ExprType::Tuple(_) => todo!(),
            ExprType::Block(_) => todo!(),
            ExprType::If {
                cond,
                then,
                otherwise,
            } => todo!(),
            ExprType::While { cond, body } => todo!(),
            ExprType::Function {
                params,
                ret_type,
                body,
            } => todo!(),
        }

        Ok(())
    }
}
