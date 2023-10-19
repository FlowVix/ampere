use std::rc::Rc;

use lasso::{Rodeo, Spur};

use crate::{
    parser::{
        ast::{ExprNode, ExprType, LetPatternNode, LetPatternType, StmtNode, StmtType},
        operators::{BinOp, UnaryOp},
    },
    source::{AmpereSource, CodeArea, CodeSpan},
    util::slabmap::SlabMap,
};

use self::{
    builder::CodeBuilder,
    bytecode::Constant,
    error::CompilerError,
    opcodes::Opcode,
    scope::{Scope, ScopeID, VarData},
};

pub mod builder;
pub mod bytecode;
pub mod error;
pub mod opcodes;
pub mod proto;
pub mod scope;

pub struct Compiler<'a> {
    src: &'a Rc<AmpereSource>,
    interner: &'a mut Rodeo,
    scopes: SlabMap<ScopeID, Scope>,
}

pub type CompileResult<T> = Result<T, CompilerError>;

impl<'a> Compiler<'a> {
    pub fn resolve(&self, s: &Spur) -> &str {
        self.interner.resolve(s)
    }
    pub fn get_var(&self, var: &Spur, scope: ScopeID) -> Option<VarData> {
        match self.scopes[scope].vars.get(var) {
            Some(v) => Some(*v),
            None => match self.scopes[scope].parent {
                Some(s) => self.get_var(var, s),
                None => None,
            },
        }
    }
    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            src: self.src.clone(),
        }
    }

    pub fn do_let(
        &mut self,
        pat: &LetPatternNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match pat.typ {
            LetPatternType::Var(v) => {
                let id = builder.new_var();
                builder.push_raw_opcode(Opcode::SetVar(id), pat.span);
            }
            LetPatternType::ArrayDestructure(_) => todo!(),
            LetPatternType::TupleDestructure(_) => todo!(),
        }
        Ok(())
    }

    pub fn compile_expr(
        &mut self,
        expr: &ExprNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &expr.typ {
            ExprType::Int(v) => builder.load_const(Constant::Int(*v), expr.span),
            ExprType::Float(v) => builder.load_const(Constant::Float(*v), expr.span),
            ExprType::String(v) => {
                builder.load_const(Constant::String(self.resolve(v).into()), expr.span)
            }
            ExprType::Bool(v) => builder.load_const(Constant::Bool(*v), expr.span),
            ExprType::Var(name) => match self.get_var(name, scope) {
                Some(v) => builder.push_raw_opcode(Opcode::LoadVar(v.id), expr.span),
                None => {
                    return Err(CompilerError::NonexistentVariable(
                        self.resolve(name).into(),
                        self.make_area(expr.span),
                    ))
                }
            },
            ExprType::Unary(op, v) => {
                self.compile_expr(v, builder, scope)?;
                match op {
                    UnaryOp::Not => builder.push_raw_opcode(Opcode::UnaryNot, expr.span),
                    UnaryOp::Minus => builder.push_raw_opcode(Opcode::UnaryMinus, expr.span),
                }
            }
            ExprType::Op(a, op, b) => {
                self.compile_expr(a, builder, scope)?;
                self.compile_expr(b, builder, scope)?;
                match op {
                    BinOp::Plus => builder.push_raw_opcode(Opcode::Plus, expr.span),
                    BinOp::Minus => builder.push_raw_opcode(Opcode::Minus, expr.span),
                    BinOp::Mult => builder.push_raw_opcode(Opcode::Mult, expr.span),
                    BinOp::Div => builder.push_raw_opcode(Opcode::Div, expr.span),
                    BinOp::Mod => builder.push_raw_opcode(Opcode::Modulo, expr.span),
                    BinOp::Pow => builder.push_raw_opcode(Opcode::Pow, expr.span),
                    BinOp::Eq => builder.push_raw_opcode(Opcode::Eq, expr.span),
                    BinOp::NotEq => builder.push_raw_opcode(Opcode::NotEq, expr.span),
                    BinOp::Gt => builder.push_raw_opcode(Opcode::Gt, expr.span),
                    BinOp::Gte => builder.push_raw_opcode(Opcode::Gte, expr.span),
                    BinOp::Lt => builder.push_raw_opcode(Opcode::Lt, expr.span),
                    BinOp::Lte => builder.push_raw_opcode(Opcode::Lte, expr.span),
                }
            }
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
    pub fn compile_stmt(
        &mut self,
        stmt: &StmtNode,
        builder: &mut CodeBuilder,
        scope: ScopeID,
    ) -> CompileResult<()> {
        match &stmt.typ {
            StmtType::Expr(v) => {
                self.compile_expr(v, builder, scope)?;
            }
            StmtType::Let(pat, v) => {}
            StmtType::Assign(_, _) => todo!(),
            StmtType::AssignOp(_, _, _) => todo!(),
            StmtType::Dbg(_) => todo!(),
        }
        Ok(())
    }
}
