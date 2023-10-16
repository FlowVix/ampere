use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;

use crate::{
    parser::{
        ast::{ExprNode, ExprType, StmtNode, StmtType, Stmts},
        operators::BinOp,
    },
    source::AmpereSource,
};

use self::{
    error::RuntimeError,
    scope::{Scope, ScopeRef},
    value::{StoredValue, Value},
};

pub mod error;
pub mod scope;
pub mod value;
pub mod value_ops;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct Interpreter<'a> {
    src: &'a Rc<AmpereSource>,
}

impl<'a> Interpreter<'a> {
    pub fn new(src: &'a Rc<AmpereSource>) -> Self {
        Self { src }
    }

    pub fn run_expr(&mut self, expr: &ExprNode, scope: &ScopeRef) -> RuntimeResult<StoredValue> {
        Ok(match &expr.typ {
            ExprType::Int(v) => Value::Int(*v),
            ExprType::Float(v) => Value::Float(*v),
            ExprType::String(v) => Value::String(v.clone()),
            ExprType::Bool(v) => Value::Bool(*v),
            ExprType::Var(name) => match scope.borrow().get_var(name) {
                Some(v) => return Ok(v),
                None => {
                    return Err(RuntimeError::NonexistentVariable(
                        name.clone(),
                        expr.span.into_area(self.src.clone()),
                    ))
                }
            },
            ExprType::Unary(_, _) => todo!(),
            ExprType::Op(a, op, b) => {
                let a = self.run_expr(a, scope)?;
                let b = self.run_expr(b, scope)?;
                match op {
                    BinOp::Plus => value_ops::plus(&a, &b, expr.span, self.src)?,
                    BinOp::Minus => value_ops::minus(&a, &b, expr.span, self.src)?,
                    BinOp::Mult => value_ops::mult(&a, &b, expr.span, self.src)?,
                    BinOp::Div => value_ops::div(&a, &b, expr.span, self.src)?,
                    BinOp::Eq => value_ops::eq(&a, &b, expr.span, self.src)?,
                    BinOp::NotEq => value_ops::neq(&a, &b, expr.span, self.src)?,
                    BinOp::Gt => todo!(),
                    BinOp::Gte => todo!(),
                    BinOp::Lt => todo!(),
                    BinOp::Lte => todo!(),
                    BinOp::Mod => todo!(),
                    BinOp::Pow => todo!(),
                }
            }
            ExprType::Index { base, index } => todo!(),
            ExprType::Member { base, member } => todo!(),
            ExprType::Array(v) => {
                let mut out = vec![];
                for i in v {
                    out.push(self.run_expr(i, scope)?.value);
                }
                Value::Array(out)
            }
            ExprType::Tuple(v) => {
                let mut out = vec![];
                for i in v {
                    out.push(self.run_expr(i, scope)?.value);
                }
                Value::Tuple(out)
            }
        }
        .into_stored(expr.span.into_area(self.src.clone())))
    }
    pub fn run_stmt(
        &mut self,
        stmt: &StmtNode,
        scope: &ScopeRef,
    ) -> RuntimeResult<Option<StoredValue>> {
        match &stmt.typ {
            StmtType::Expr(e) => return self.run_expr(e, scope).map(Some),
            StmtType::Let(name, e) => {
                let v = self.run_expr(e, scope)?;
                scope.borrow_mut().vars.insert(name.clone(), v);
            }
        }
        Ok(None)
    }
    pub fn run_stmts(
        &mut self,
        stmts: &Stmts,
        scope: &ScopeRef,
    ) -> RuntimeResult<Option<StoredValue>> {
        let (v, ret) = match stmts {
            Stmts::Normal(v) => (v, None),
            Stmts::Ret(v, ret) => (v, Some(ret)),
        };

        for stmt in v {
            self.run_stmt(stmt, scope)?;
        }
        Ok(if let Some(ret) = ret {
            self.run_stmt(ret, scope)?
        } else {
            None
        })
    }
    pub fn new_run_file(
        stmts: &Stmts,
        src: &'a Rc<AmpereSource>,
    ) -> RuntimeResult<Option<StoredValue>> {
        let mut interpreter = Self::new(src);
        let global_scope = ScopeRef::new(Scope {
            vars: AHashMap::new(),
            parent: None,
        });
        interpreter.run_stmts(stmts, &global_scope)
    }
}
