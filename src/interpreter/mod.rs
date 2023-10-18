use std::{cell::RefCell, rc::Rc};

use ahash::AHashMap;
use colored::Colorize;
use itertools::Itertools;

use crate::{
    parser::{
        ast::{
            AssignPatternNode, AssignPatternType, ExprNode, ExprType, LetPatternNode,
            LetPatternType, StmtNode, StmtType, Stmts,
        },
        operators::BinOp,
    },
    source::AmpereSource,
};

use self::{
    error::RuntimeError,
    scope::{Scope, ScopeRef},
    value::{StoredValue, Value, ValueRef, ValueType},
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

    pub fn do_let(
        &mut self,
        pat: &LetPatternNode,
        val: &ValueRef,
        scope: &ScopeRef,
    ) -> RuntimeResult<()> {
        match &pat.typ {
            LetPatternType::Var(name) => {
                scope
                    .borrow_mut()
                    .vars
                    .insert(name.clone(), val.deep_clone());
            }
            LetPatternType::ArrayDestructure(pats) | LetPatternType::TupleDestructure(pats) => {
                let val = val.borrow();

                let is_tuple = matches!(pat.typ, LetPatternType::TupleDestructure(_));

                let v = match (&val.value, is_tuple) {
                    (Value::Array(v), false) => v,
                    (Value::Tuple(v), true) => v,
                    (v, _) => {
                        return Err(RuntimeError::TypeMismatch {
                            v: (v.get_type(), val.def_area.clone()),
                            expected: if is_tuple {
                                ValueType::Tuple
                            } else {
                                ValueType::Array
                            },
                            area: pat.span.into_area(self.src.clone()),
                        })
                    }
                };

                if pats.len() != v.len() {
                    return Err(RuntimeError::DestructureLenMismatch {
                        expected: pats.len(),
                        found: v.len(),
                        val_area: val.def_area.clone(),
                        area: pat.span.into_area(self.src.clone()),
                    });
                }
                for (pat, v) in pats.iter().zip(v) {
                    self.do_let(pat, v, scope)?;
                }
            }
        }
        Ok(())
    }
    pub fn do_assign(
        &mut self,
        pat: &AssignPatternNode,
        val: &ValueRef,
        scope: &ScopeRef,
    ) -> RuntimeResult<()> {
        match &pat.typ {
            AssignPatternType::Path { var, path } => match scope.borrow().get_var(var) {
                Some(v) => {
                    *v.borrow_mut() = val.deep_clone().borrow().clone();
                }
                None => {
                    return Err(RuntimeError::NonexistentVariable(
                        var.clone(),
                        pat.span.into_area(self.src.clone()),
                    ))
                }
            },
            AssignPatternType::ArrayDestructure(pats)
            | AssignPatternType::TupleDestructure(pats) => {
                let val = val.borrow();

                let is_tuple = matches!(pat.typ, AssignPatternType::TupleDestructure(_));

                let v = match (&val.value, is_tuple) {
                    (Value::Array(v), false) => v,
                    (Value::Tuple(v), true) => v,
                    (v, _) => {
                        return Err(RuntimeError::TypeMismatch {
                            v: (v.get_type(), val.def_area.clone()),
                            expected: if is_tuple {
                                ValueType::Tuple
                            } else {
                                ValueType::Array
                            },
                            area: pat.span.into_area(self.src.clone()),
                        })
                    }
                };

                if pats.len() != v.len() {
                    return Err(RuntimeError::DestructureLenMismatch {
                        expected: pats.len(),
                        found: v.len(),
                        val_area: val.def_area.clone(),
                        area: pat.span.into_area(self.src.clone()),
                    });
                }
                for (pat, v) in pats.iter().zip(v) {
                    self.do_assign(pat, v, scope)?;
                }
            }
        }
        Ok(())
    }

    pub fn run_expr(&mut self, expr: &ExprNode, scope: &ScopeRef) -> RuntimeResult<ValueRef> {
        Ok(ValueRef::new(
            match &expr.typ {
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
                        BinOp::Plus => {
                            value_ops::plus(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Minus => {
                            value_ops::minus(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Mult => {
                            value_ops::mult(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Div => {
                            value_ops::div(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Eq => value_ops::eq(&a.borrow(), &b.borrow(), expr.span, self.src)?,
                        BinOp::NotEq => {
                            value_ops::neq(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Gt => value_ops::gt(&a.borrow(), &b.borrow(), expr.span, self.src)?,
                        BinOp::Gte => {
                            value_ops::gte(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Lt => value_ops::lt(&a.borrow(), &b.borrow(), expr.span, self.src)?,
                        BinOp::Lte => {
                            value_ops::lte(&a.borrow(), &b.borrow(), expr.span, self.src)?
                        }
                        BinOp::Mod => todo!(),
                        BinOp::Pow => todo!(),
                    }
                }
                ExprType::Index { base, index } => todo!(),
                ExprType::Member { base, member } => todo!(),
                ExprType::Array(v) => {
                    let mut out = vec![];
                    for i in v {
                        out.push(self.run_expr(i, scope)?.deep_clone());
                    }
                    Value::Array(out)
                }
                ExprType::Tuple(v) => {
                    let mut out = vec![];
                    for i in v {
                        out.push(self.run_expr(i, scope)?.deep_clone());
                    }
                    Value::Tuple(out)
                }
                ExprType::Block(stmts) => {
                    let derived = scope.derived();
                    return self.run_stmts(stmts, &derived).map(|v| {
                        v.unwrap_or(ValueRef::new(
                            Value::unit().into_stored(expr.span.into_area(self.src.clone())),
                        ))
                    });
                }
                ExprType::If {
                    cond,
                    then,
                    otherwise,
                } => {
                    if value_ops::as_bool(
                        &self.run_expr(cond, scope)?.borrow(),
                        expr.span,
                        self.src,
                    )? {
                        return self.run_expr(then, scope);
                    }
                    if let Some(b) = otherwise {
                        return self.run_expr(b, scope);
                    }
                    Value::unit()
                }
                ExprType::While { cond, body } => {
                    let mut ret = None;
                    while value_ops::as_bool(
                        &self.run_expr(cond, scope)?.borrow(),
                        expr.span,
                        self.src,
                    )? {
                        ret = Some(self.run_expr(body, scope)?);
                    }
                    if let Some(v) = ret {
                        return Ok(v);
                    }
                    Value::unit()
                }
                ExprType::Associated { base, member } => todo!(),
                ExprType::Call { base, args } => todo!(),
                ExprType::Function {
                    params,
                    ret_type,
                    body,
                } => {
                    let mut out_params = vec![];
                    for (pat, t) in params {
                        let t = if let Some(t) = t {
                            Some(self.run_expr(t, scope)?)
                        } else {
                            None
                        };
                        out_params.push((pat.clone(), t))
                    }
                    let ret_type = if let Some(t) = ret_type {
                        Some(self.run_expr(t, scope)?)
                    } else {
                        None
                    };
                    Value::Function {
                        params: out_params,
                        ret_type,
                        body: (**body).clone(),
                        parent_scope: scope.clone(),
                    }
                }
            }
            .into_stored(expr.span.into_area(self.src.clone())),
        ))
    }
    pub fn run_stmt(
        &mut self,
        stmt: &StmtNode,
        scope: &ScopeRef,
    ) -> RuntimeResult<Option<ValueRef>> {
        match &stmt.typ {
            StmtType::Expr(e) => return self.run_expr(e, scope).map(Some),
            // StmtType::Let(name, e) => {
            //     let v = self.run_expr(e, scope)?;
            //     scope.borrow_mut().vars.insert(name.clone(), v);
            // }
            StmtType::Dbg(v) => {
                println!(
                    "{}",
                    self.run_expr(v, scope)?
                        .borrow()
                        .value
                        .to_str()
                        .bright_green()
                )
            }
            StmtType::Assign(p, e) => {
                let val = self.run_expr(e, scope)?;
                self.do_assign(p, &val, scope)?;
            }
            StmtType::Let(p, e) => {
                let val = self.run_expr(e, scope)?;
                self.do_let(p, &val, scope)?;
            }
            StmtType::AssignOp(_, _, _) => todo!(),
        }
        Ok(None)
    }
    pub fn run_stmts(
        &mut self,
        stmts: &Stmts,
        scope: &ScopeRef,
    ) -> RuntimeResult<Option<ValueRef>> {
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
    ) -> RuntimeResult<Option<ValueRef>> {
        let mut interpreter = Self::new(src);
        let global_scope = ScopeRef::new(Scope {
            vars: AHashMap::new(),
            parent: None,
        });
        interpreter.run_stmts(stmts, &global_scope)
    }
}
