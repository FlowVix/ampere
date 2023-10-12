use std::{cell::RefCell, rc::Rc};

use crate::source::CodeArea;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Array(Vec<Value>),
    Tuple(Vec<Value>),
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
}
