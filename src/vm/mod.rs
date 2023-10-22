use std::rc::Rc;

use colored::Colorize;
use itertools::Itertools;

use crate::{
    compiler::{
        bytecode::{Bytecode, Constant, Function},
        opcodes::Opcode,
    },
    source::{AmpereSource, CodeArea, CodeSpan},
    util::ImmutVec,
    vm::value::ValueType,
};

use self::{
    error::RuntimeError,
    memory::{MemKey, Memory},
    value::{StoredValue, Value},
};

pub mod error;
pub mod memory;
pub mod value;
pub mod value_ops;

// pub type Program = ImmutVec<Bytecode>;

pub struct Vm {
    pub memory: Memory,

    pub stack: Vec<MemKey>,
}

#[derive(Clone, Copy)]
pub struct RunInfo<'a> {
    pub program: &'a ImmutVec<Bytecode>,
    pub bytecode_idx: usize,
    pub func_idx: usize,
}
impl<'a> RunInfo<'a> {
    #[inline]
    pub fn bytecode(&self) -> &Bytecode {
        &self.program[self.bytecode_idx]
    }
    #[inline]
    pub fn func(&self) -> &Function {
        &self.bytecode().funcs[self.func_idx]
    }
    #[inline]
    pub fn opcodes(&self) -> &[Opcode] {
        &self.func().opcodes
    }
    #[inline]
    pub fn src(&self) -> &Rc<AmpereSource> {
        &self.bytecode().src
    }
    #[inline]
    pub fn constants(&self) -> &[Constant] {
        &self.bytecode().constants
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

impl Vm {
    #[inline]
    pub fn store_value(&mut self, s: StoredValue) -> MemKey {
        self.memory.insert(s)
    }
    #[inline]
    pub fn push_value(&mut self, s: StoredValue) -> MemKey {
        let k = self.store_value(s);
        self.stack.push(k);
        k
    }
    #[inline]
    pub fn push_key(&mut self, k: MemKey) {
        self.stack.push(k);
    }
    #[inline]
    pub fn pop(&mut self) -> MemKey {
        self.stack.pop().unwrap()
    }
    #[inline]
    pub fn last(&self) -> MemKey {
        *self.stack.last().unwrap()
    }
    #[inline]
    pub fn try_pop(&mut self) -> Option<MemKey> {
        self.stack.pop()
    }
    #[inline]
    pub fn get(&self, k: MemKey) -> &StoredValue {
        &self.memory[k]
    }
    #[inline]
    pub fn get_mut(&mut self, k: MemKey) -> &mut StoredValue {
        &mut self.memory[k]
    }
    pub fn deep_clone(&mut self, k: MemKey) -> Value {
        match self.get(k).value.clone() {
            Value::Array(mut v) => {
                for k in &mut v {
                    *k = self.deep_clone_key(*k);
                }
                Value::Array(v)
            }
            Value::Tuple(mut v) => {
                for k in &mut v {
                    *k = self.deep_clone_key(*k);
                }
                Value::Tuple(v)
            }
            v => v,
        }
    }
    pub fn deep_clone_stored(&mut self, k: MemKey) -> StoredValue {
        let v = self.deep_clone(k);
        StoredValue {
            value: v,
            def_area: self.get(k).def_area.clone(),
        }
    }
    pub fn deep_clone_key(&mut self, k: MemKey) -> MemKey {
        let v = self.deep_clone_stored(k);
        self.store_value(v)
    }
    pub fn new_var_vec(&mut self, len: usize, info: RunInfo) -> Vec<MemKey> {
        std::iter::from_fn(|| {
            Some(self.store_value(
                Value::unit().into_stored(CodeSpan::ZEROSPAN.into_area(info.src().clone())),
            ))
        })
        .take(len)
        .collect()
    }

    pub fn run_func(&mut self, info: RunInfo, vars: Option<Vec<MemKey>>) -> RuntimeResult<()> {
        let mut pos = 0;

        macro_rules! opcode {
            (Span) => {
                info.func().opcode_spans[pos]
            };
            (Area) => {
                opcode!(Span).into_area(info.src().clone())
            };
        }

        let mut vars =
            vars.unwrap_or_else(|| self.new_var_vec(info.func().var_count as usize, info));

        macro_rules! bin_op {
            ($fn:ident) => {{
                let b = self.pop();
                let a = self.pop();
                let result = value_ops::$fn(self, a, b, opcode!(Span), info.src())?;
                self.push_value(result.into_stored(opcode!(Area)));
            }};
        }

        while pos < info.opcodes().len() {
            match info.opcodes()[pos] {
                Opcode::LoadConst(v) => {
                    self.push_value(
                        Value::from_const(&info.constants()[*v as usize])
                            .into_stored(opcode!(Area)),
                    );
                }
                Opcode::SetVar(v) => {
                    let k = self.pop();
                    self.memory[vars[*v as usize]] = self.deep_clone_stored(k);
                    // vars[*v as usize] = self.deep_clone_key(k);
                }
                Opcode::LoadVar(v) => self.push_key(vars[*v as usize]),
                Opcode::Plus => bin_op!(plus),
                Opcode::Minus => bin_op!(minus),
                Opcode::Mult => bin_op!(mult),
                Opcode::Div => bin_op!(div),
                Opcode::Modulo => bin_op!(modulo),
                Opcode::Pow => bin_op!(pow),
                Opcode::Gt => bin_op!(gt),
                Opcode::Gte => bin_op!(gte),
                Opcode::Lt => bin_op!(lt),
                Opcode::Lte => bin_op!(lte),
                Opcode::Eq => bin_op!(eq),
                Opcode::NotEq => bin_op!(not_eq),
                Opcode::UnaryMinus => todo!(),
                Opcode::UnaryNot => todo!(),
                Opcode::Jump(v) => {
                    pos = *v as usize;
                    continue;
                }
                Opcode::JumpIfFalse(v) => {
                    let top = self.pop();
                    if !value_ops::as_bool(self, top, opcode!(Span), info.src())? {
                        pos = *v as usize;
                        continue;
                    }
                }
                Opcode::JumpIfTrue(v) => {
                    let top = self.pop();
                    if value_ops::as_bool(self, top, opcode!(Span), info.src())? {
                        pos = *v as usize;
                        continue;
                    }
                }
                Opcode::PopTop => {
                    self.pop();
                }
                Opcode::PushUnit => {
                    self.push_value(Value::unit().into_stored(opcode!(Area)));
                }
                Opcode::WrapArray(len) => {
                    let mut v = vec![0.into(); len as usize];

                    for i in (0..len).rev() {
                        let k = self.pop();
                        v[i as usize] = self.deep_clone_key(k);
                    }

                    self.push_value(Value::Array(v).into_stored(opcode!(Area)));
                }
                Opcode::WrapTuple(len) => {
                    let mut v = vec![0.into(); len as usize];

                    for i in (0..len).rev() {
                        let k = self.pop();
                        v[i as usize] = self.deep_clone_key(k);
                    }

                    self.push_value(Value::Tuple(v).into_stored(opcode!(Area)));
                }
                Opcode::Dbg => {
                    let v = self.last();
                    println!("{}", self.value_str(v, true))
                }
                o @ (Opcode::UnwrapArray(len) | Opcode::UnwrapTuple(len)) => {
                    let top = self.pop();
                    let top = self.get(top);

                    match (&top.value, matches!(o, Opcode::UnwrapTuple(_))) {
                        (Value::Array(v), false) | (Value::Tuple(v), true) => {
                            if v.len() != len as usize {
                                return Err(RuntimeError::DestructureLenMismatch {
                                    expected: len as usize,
                                    found: v.len(),
                                    val_area: top.def_area.clone(),
                                    area: opcode!(Area),
                                });
                            }
                            for i in v.clone() {
                                self.push_key(i)
                            }
                        }
                        (_, t) => {
                            return Err(RuntimeError::TypeMismatch {
                                v: (top.value.get_type(), top.def_area.clone()),
                                expected: if t {
                                    ValueType::Tuple
                                } else {
                                    ValueType::Array
                                },
                                area: opcode!(Area),
                            })
                        }
                    };
                }
                Opcode::PushFunc(id) => {
                    self.push_value(
                        Value::Function {
                            func: id,
                            param_types: vec![],
                            ret_type: None,
                            captured: info.bytecode().funcs[*id as usize]
                                .captured
                                .iter()
                                .map(|(from, _)| vars[**from as usize])
                                .collect(),
                        }
                        .into_stored(opcode!(Area)),
                    );
                }
                Opcode::SetArgAmount(n) => {
                    if let Value::Function { param_types, .. } =
                        &mut self.get_mut(self.last()).value
                    {
                        *param_types = vec![None; n as usize]
                    };
                }
                Opcode::SetArgType(arg) => {
                    let t = self.pop();
                    if let Value::Function { param_types, .. } =
                        &mut self.get_mut(self.last()).value
                    {
                        param_types[arg as usize] = Some(t)
                    };
                }
                Opcode::SetReturnType => {
                    let t = self.pop();
                    if let Value::Function { ret_type, .. } = &mut self.get_mut(self.last()).value {
                        *ret_type = Some(t)
                    };
                }
                Opcode::Call(args) => {
                    // let mut v = vec![0.into(); args as usize];

                    // for i in (0..args).rev() {
                    //     let k = self.pop();
                    //     v[i as usize] = k;
                    // }

                    // let base = self.pop();

                    self.call_value(args as usize, opcode!(Area), info)?;
                }
            }

            pos += 1;
        }

        Ok(())
    }

    pub fn value_str(&self, k: MemKey, debug: bool) -> String {
        let s = match &self.get(k).value {
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => v.to_string(),
            Value::String(v) => v.clone(),
            Value::Array(v) => format!(
                "[{}]",
                v.iter().map(|v| self.value_str(*v, debug)).join(", ")
            ),
            Value::Tuple(v) => format!(
                "({})",
                v.iter().map(|v| self.value_str(*v, debug)).join(", ")
            ),
            Value::Type(t) => format!("<{}>", t.name()),
            Value::Function {
                ret_type,
                param_types,
                ..
            } => {
                format!(
                    "({}){} => ...",
                    param_types
                        .iter()
                        .map(|v| if let Some(t) = v {
                            self.value_str(*t, debug)
                        } else {
                            "_".into()
                        })
                        .join(", "),
                    if let Some(t) = ret_type {
                        format!("-> {}", self.value_str(*t, debug))
                    } else {
                        "".into()
                    },
                )
            }
        };
        if !debug {
            return s;
        }
        format!("{}{}", s, format!("::k{:?}", k.0).dimmed())
    }

    pub fn call_value(
        &mut self,
        arg_amount: usize,
        call_area: CodeArea,
        info: RunInfo,
    ) -> RuntimeResult<()> {
        // {
        //     let l = self.stack.len();
        //     self.stack[(l - 1 - arg_amount)..].rotate_left(1);
        // }
        // self.stack.remo

        let base = self.stack.remove(self.stack.len() - 1 - arg_amount);
        let base = &self.memory[base];
        match &base.value {
            Value::Function {
                func,
                param_types,
                ret_type,
                captured,
            } => {
                if arg_amount != param_types.len() {
                    return Err(RuntimeError::IncorrectArgAmount {
                        expected: param_types.len(),
                        found: arg_amount,
                        val_area: base.def_area.clone(),
                        area: call_area,
                    });
                }
                let func = *func;
                let captured = captured.clone();
                let new_info = RunInfo {
                    func_idx: *func as usize,
                    ..info
                };

                let mut vars = self.new_var_vec(new_info.func().var_count as usize, new_info);
                for (k, (_, to)) in captured.iter().zip(new_info.func().captured.iter()) {
                    vars[**to as usize] = *k;
                }

                self.run_func(
                    RunInfo {
                        func_idx: *func as usize,
                        ..info
                    },
                    Some(vars),
                )?;
            }
            _ => {
                return Err(RuntimeError::CannotCall {
                    v: (base.value.get_type(), base.def_area.clone()),
                    area: call_area,
                })
            }
        }
        Ok(())
    }
}
