use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use itertools::Itertools;

use crate::source::CodeArea;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Array(Vec<ValueRef>),
    Tuple(Vec<ValueRef>),
}

impl Value {
    pub fn display(&self) -> String {
        match self {
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => v.to_string(),
            Value::String(v) => v.clone(),
            Value::Array(v) => format!(
                "[{}]",
                v.iter().map(|v| v.borrow().value.display()).join(", ")
            ),
            Value::Tuple(v) => format!(
                "({})",
                v.iter().map(|v| v.borrow().value.display()).join(", ")
            ),
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

#[derive(Debug, Clone, PartialEq, derive_more::Deref, derive_more::DerefMut)]
pub struct ValueRef(Rc<RefCell<StoredValue>>);

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
            Value::Array(v) => ValueType::Array,
            Value::Tuple(v) => ValueType::Tuple,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FullType {
    Known(ValueType),
    Unknown,
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
        }
    }
    pub fn into_known(self) -> FullType {
        FullType::Known(self)
    }
}

impl FullType {
    pub fn name(&self) -> String {
        match self {
            FullType::Known(k) => k.name(),
            FullType::Unknown => "?".into(),
        }
    }
}
