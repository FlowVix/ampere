use std::rc::Rc;

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
    memory: Memory,

    stack: Vec<MemKey>,
}

#[derive(Clone, Copy)]
pub struct RunInfo<'a> {
    program: &'a ImmutVec<Bytecode>,
    bytecode_idx: usize,
    func_idx: usize,
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

        let mut vars = Vec::with_capacity(info.func().var_count as usize);

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
                Opcode::Gt => bin_op!(gt),
                Opcode::Gte => bin_op!(gte),
                Opcode::Lt => bin_op!(lt),
                Opcode::Lte => bin_op!(lte),
                Opcode::Eq => bin_op!(eq),
                Opcode::NEq => bin_op!(neq),
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
            }

            pos += 1;
        }

        Ok(())
    }
}
