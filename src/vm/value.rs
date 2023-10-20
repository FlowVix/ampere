use itertools::Itertools;

use crate::{
    compiler::bytecode::Constant,
    parser::ast::{ExprNode, LetPatternNode},
    source::CodeArea,
};

use super::{memory::MemKey, Vm};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Array(Vec<MemKey>),
    Tuple(Vec<MemKey>),

    Type(ValueType),
}

impl Value {
    pub fn into_stored(self, def_area: CodeArea) -> StoredValue {
        StoredValue {
            value: self,
            def_area,
        }
    }
    pub fn from_const(c: &Constant) -> Self {
        match c {
            Constant::Int(v) => Self::Int(*v),
            Constant::Float(v) => Self::Float(*v),
            Constant::String(v) => Self::String(v.to_string()),
            Constant::Bool(v) => Self::Bool(*v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StoredValue {
    pub value: Value,
    pub def_area: CodeArea,
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
            // Value::Function { .. } => ValueType::Function,
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
    // Function,
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
            // ValueType::Function => "function".into(),
        }
    }
}
