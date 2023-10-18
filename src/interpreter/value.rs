use std::{
    cell::{Ref, RefCell},
    rc::{Rc, Weak},
};

use itertools::Itertools;

use crate::{
    parser::ast::{ExprNode, LetPatternNode},
    source::CodeArea,
};

use super::scope::ScopeRef;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Array(Vec<ValueRef>),
    Tuple(Vec<ValueRef>),

    Type(ValueType),

    Function {
        params: Vec<(LetPatternNode, Option<ValueRef>)>,
        ret_type: Option<ValueRef>,
        body: ExprNode,
        parent_scope: ScopeRef,
    },
}

impl Value {
    pub fn to_str(&self) -> String {
        match self {
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => v.to_string(),
            Value::String(v) => v.clone(),
            Value::Array(v) => format!(
                "[{}]",
                v.iter().map(|v| v.borrow().value.to_str()).join(", ")
            ),
            Value::Tuple(v) => format!(
                "({})",
                v.iter().map(|v| v.borrow().value.to_str()).join(", ")
            ),
            Value::Type(t) => format!("<{}>", t.name()),
            Value::Function {
                params, ret_type, ..
            } => {
                format!(
                    "({}){} => ...",
                    params
                        .iter()
                        .map(|(pat, typ)| {
                            format!(
                                "{}{}",
                                pat.to_str(),
                                if let Some(t) = typ {
                                    format!(": {}", t.borrow().value.to_str())
                                } else {
                                    "".into()
                                }
                            )
                        })
                        .join(", "),
                    if let Some(t) = ret_type {
                        format!("-> {}", t.borrow().value.to_str())
                    } else {
                        "".into()
                    },
                )
            }
        }
    }
    pub fn into_stored(self, def_area: CodeArea) -> StoredValue {
        StoredValue {
            value: self,
            def_area,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StoredValue {
    pub value: Value,
    pub def_area: CodeArea,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    derive_more::Deref,
    derive_more::DerefMut,
    derive_more::Into,
    derive_more::From,
)]
pub struct ValueRef(Rc<RefCell<StoredValue>>);
#[derive(
    Debug, Clone, derive_more::Deref, derive_more::DerefMut, derive_more::Into, derive_more::From,
)]
pub struct ValueWeak(Weak<RefCell<StoredValue>>);

impl ValueRef {
    pub fn new(v: StoredValue) -> Self {
        Self(Rc::new(RefCell::new(v)))
    }
    pub fn deep_clone(&self) -> Self {
        let v = self.borrow();

        Self::new(
            match &v.value {
                Value::Array(v) => Value::Array(v.iter().map(|v| v.deep_clone()).collect()),
                Value::Tuple(v) => Value::Tuple(v.iter().map(|v| v.deep_clone()).collect()),
                v => v.clone(),
            }
            .into_stored(v.def_area.clone()),
        )
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Bool(_) => ValueType::Bool,
            Value::String(_) => ValueType::String,
            Value::Array(_) => ValueType::Array,
            Value::Tuple(_) => ValueType::Tuple,
            Value::Type(_) => ValueType::Type,
            Value::Function { .. } => ValueType::Function,
        }
    }
    pub fn unit() -> Self {
        Self::Tuple(vec![])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueType {
    Int,
    Float,
    Bool,
    String,
    Array,
    Tuple,
    Type,
    Function,
}

impl ValueType {
    pub fn name(&self) -> String {
        match self {
            ValueType::Int => "int".into(),
            ValueType::Float => "float".into(),
            ValueType::Bool => "bool".into(),
            ValueType::String => "string".into(),
            ValueType::Array => "array".into(),
            ValueType::Tuple => "tuple".into(),
            ValueType::Type => "type".into(),
            ValueType::Function => "function".into(),
        }
    }
}
