use std::rc::Rc;

use itertools::Itertools;

use crate::source::{AmpereSource, CodeArea, CodeSpan};

use super::{
    error::RuntimeError,
    memory::MemKey,
    value::{StoredValue, Value},
    RuntimeResult, Vm,
};

pub fn test(m: &mut Vm, a: MemKey, b: MemKey, span: CodeSpan) {}

pub fn plus(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a + *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a + *b),
        (Value::String(a), Value::String(b)) => Value::String(a.clone() + b),
        (Value::Array(a), Value::Array(b)) => {
            let mut out = a.iter().chain(b).copied().collect_vec();
            for k in &mut out {
                *k = vm.deep_clone_key(*k)
            }

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
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
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
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(*a * *b),
        (Value::Float(a), Value::Float(b)) => Value::Float(*a * *b),
        (Value::String(s), Value::Int(n)) | (Value::Int(n), Value::String(s)) => {
            Value::String(s.repeat((*n).max(0) as usize))
        }
        (Value::Array(v), Value::Int(n)) | (Value::Int(n), Value::Array(v)) => {
            let mut out = v
                .iter()
                .cycle()
                .take(v.len() * (*n).max(0) as usize)
                .copied()
                .collect_vec();
            for k in &mut out {
                *k = vm.deep_clone_key(*k)
            }

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
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
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

pub fn modulo(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int(a.rem_euclid(*b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.rem_euclid(*b)),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "%",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn pow(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Int((*a as f64).powf(*b as f64) as i64),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(*b)),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: "**",
                area: span.into_area(src.clone()),
            })
        }
    })
}

pub fn gt(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Bool(*a > *b),
        (Value::Float(a), Value::Float(b)) => Value::Bool(*a > *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: ">",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn gte(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Bool(*a >= *b),
        (Value::Float(a), Value::Float(b)) => Value::Bool(*a >= *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: ">",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn lt(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Bool(*a < *b),
        (Value::Float(a), Value::Float(b)) => Value::Bool(*a < *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: ">",
                area: span.into_area(src.clone()),
            })
        }
    })
}
pub fn lte(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    Ok(match (&a.value, &b.value) {
        (Value::Int(a), Value::Int(b)) => Value::Bool(*a <= *b),
        (Value::Float(a), Value::Float(b)) => Value::Bool(*a <= *b),
        (_, _) => {
            return Err(RuntimeError::InvalidBinaryOperands {
                a: (a.value.get_type(), a.def_area.clone()),
                b: (b.value.get_type(), b.def_area.clone()),
                op: ">",
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

pub fn as_bool(
    vm: &mut Vm,
    v: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<bool> {
    let v = vm.get(v);
    match &v.value {
        Value::Bool(b) => Ok(*b),
        _ => Err(RuntimeError::NonBoolCondition {
            v: (v.value.get_type(), v.def_area.clone()),
            area: span.into_area(src.clone()),
        }),
    }
}

pub fn eq(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    equality(&a.value, &b.value)
        .map(Value::Bool)
        .ok_or(RuntimeError::InvalidBinaryOperands {
            a: (a.value.get_type(), a.def_area.clone()),
            b: (b.value.get_type(), b.def_area.clone()),
            op: "==",
            area: span.into_area(src.clone()),
        })
}
pub fn not_eq(
    vm: &mut Vm,
    a: MemKey,
    b: MemKey,
    span: CodeSpan,
    src: &Rc<AmpereSource>,
) -> RuntimeResult<Value> {
    let a = vm.get(a);
    let b = vm.get(b);
    equality(&a.value, &b.value).map(|v| Value::Bool(!v)).ok_or(
        RuntimeError::InvalidBinaryOperands {
            a: (a.value.get_type(), a.def_area.clone()),
            b: (b.value.get_type(), b.def_area.clone()),
            op: "==",
            area: span.into_area(src.clone()),
        },
    )
}
