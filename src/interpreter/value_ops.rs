use std::rc::Rc;

use itertools::Itertools;

use crate::source::{AmpereSource, CodeArea, CodeSpan};

use super::{
    error::RuntimeError,
    value::{StoredValue, Value},
    RuntimeResult,
};

pub fn plus(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a + *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a + *b),
        (Value::String(a), Value::String(b)) => Value::String(a.clone() + b),
        (Value::Array(a), Value::Array(b)) => {
            let mut out = a.clone();
            out.extend(b.iter().cloned());
            Value::Array(out)
        }
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "+",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn minus(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a - *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a - *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "-",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn mult(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a * *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a * *b),
        (Value::String(s), Value::Int(n)) | (Value::Int(n), Value::String(s)) => {
            Value::String(s.repeat((*n).max(0) as usize))
        }
        (Value::Array(v), Value::Int(n)) | (Value::Int(n), Value::Array(v)) => {
            let out = v
                .iter()
                .cycle()
                .take(v.len() * (*n).max(0) as usize)
                .cloned()
                .collect_vec();
            Value::Array(out)
        }
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "*",
                area: span.into_area(src.clone()),
            })
        }
    })
}

pub fn div(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a / *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a / *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "/",
                area: span.into_area(src.clone()),
            })
        }
    })
}

fn equality(a: &Value, b: &Value) -> Option<bool> {
    if std::mem::discriminant(a) == std::mem::discriminant(b) {
        Some(a == b)
    } else {
        None
    }
}

pub fn eq(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    equality(&a.value, &b.value)
        .map(Value::Bool)
        .ok_or(RuntimeError::InvalidBinaryOperands {
            a: (a.value.get_type(), a.def_area.clone()),
            b: (b.value.get_type(), b.def_area.clone()),
            op: "==",
            area: span.into_area(src.clone()),
        })
}
pub fn neq(
    a: &StoredValue,
    b: &StoredValue,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    equality(&a.value, &b.value).map(|v| Value::Bool(!v)).ok_or(
        RuntimeError::InvalidBinaryOperands {
            a: (a.value.get_type(), a.def_area.clone()),
            b: (b.value.get_type(), b.def_area.clone()),
            op: "==",
            area: span.into_area(src.clone()),
        },
    )
}
