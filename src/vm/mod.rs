use std::rc::Rc;

use colored::Colorize;
use itertools::Itertools;

use crate::{
    compiler::{
        bytecode::{Bytecode, Constant, Function},
        opcodes::Opcode,
    },
    source::{AmpereSource, CodeSpan},
    util::ImmutVec,
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
    pub fn deep_clone_key(&mut self, k: MemKey) -> MemKey {
        let v = self.deep_clone(k);
        self.store_value(StoredValue {
            value: v,
            def_area: self.get(k).def_area.clone(),
        })
    }

    pub fn run_func(&mut self, info: RunInfo) -> RuntimeResult<()> {
        let mut pos = 0;

        macro_rules! opcode {
            (Span) => {
                info.func().opcode_spans[pos]
            };
            (Area) => {
                opcode!(Span).into_area(info.src().clone())
            };
        }

        let mut vars = vec![0.into(); info.func().var_count as usize];

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
                    vars[*v as usize] = self.deep_clone_key(k);
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
                    let v = self.pop();
                    println!("{}", self.value_str(v, true))
                }
            }

            pos += 1;
        }

        Ok(())
    }

    pub fn value_str(&self, k: MemKey, debug: bool) -> String {
        let s = match &self.memory[k].value {
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
            // Value::Function {
            //     params, ret_type, ..
            // } => {
            //     format!(
            //         "({}){} => ...",
            //         params
            //             .iter()
            //             .map(|(pat, typ)| {
            //                 format!(
            //                     "{}{}",
            //                     pat.to_str(),
            //                     if let Some(t) = typ {
            //                         format!(": {}", t.borrow().value.to_str())
            //                     } else {
            //                         "".into()
            //                     }
            //                 )
            //             })
            //             .join(", "),
            //         if let Some(t) = ret_type {
            //             format!("-> {}", t.borrow().value.to_str())
            //         } else {
            //             "".into()
            //         },
            //     )
            // }
        };
        if !debug {
            return s;
        }
        format!("{}{}", s, format!("::k{:?}", k.0).dimmed())
    }
}
