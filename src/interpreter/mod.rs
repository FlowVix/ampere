use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;

use crate::{
    parser::ast::{ExprNode, ExprType},
    source::AmpereSource,
};

use self::{
    error::RuntimeError,
    value::{StoredValue, Value, ValueRef},
};

pub mod error;
pub mod value;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub struct Scope {
    vars: AHashMap<String, ValueRef>,
    parent: Option<ScopeRef>,
}

#[derive(Debug, Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct ScopeRef(Rc<RefCell<Scope>>);

impl ScopeRef {
    pub fn new(v: Scope) -> Self {
        Self(Rc::new(RefCell::new(v)))
    }
}

impl Scope {
    pub fn get_var(&self, name: &str) -> Option<ValueRef> {
        match self.vars.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(p) => p.borrow().get_var(name),
                None => None,
            },
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn run_expr(
        &mut self,
        expr: &ExprNode,
        scope: &ScopeRef,
        src: &Rc<AmpereSource>,
    ) -> RuntimeResult<Value> {
        Ok(match &expr.typ {
            ExprType::Int(v) => Value::Int(*v),
            ExprType::Float(v) => Value::Float(*v),
            ExprType::String(v) => Value::String(v.clone()),
            ExprType::Bool(v) => Value::Bool(*v),
            ExprType::Var(name) => match scope.borrow().get_var(name) {
                Some(v) => v.borrow().value.clone(),
                None => {
                    return Err(RuntimeError::NonexistentVariable(
                        name.clone(),
                        expr.span.into_area(src.clone()),
                    ))
                }
            },
            ExprType::Unary(_, _) => todo!(),
            ExprType::Op(_, _, _) => todo!(),
            ExprType::Index { base, index } => todo!(),
            ExprType::Member { base, member } => todo!(),
        })
    }
}
