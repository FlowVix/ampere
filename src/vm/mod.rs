use std::rc::Rc;

use colored::Colorize;
use itertools::Itertools;

use crate::{
    compiler::{
        bytecode::{Bytecode, CallExpr, Constant, Function},
        opcodes::{Opcode, Register},
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
}

pub struct Registers {
    pub vec: Vec<MemKey>,
}
impl Registers {
    pub fn new(regs: usize, vm: &mut Vm, info: RunInfo) -> Self {
        Self {
            vec: std::iter::from_fn(|| {
                Some(vm.store_value(
                    Value::unit().into_stored(CodeSpan::ZEROSPAN.into_area(info.src().clone())),
                ))
            })
            .take(regs)
            .collect(),
        }
    }
    #[inline]
    pub fn set_reg(&mut self, reg: Register, v: StoredValue, vm: &mut Vm) {
        vm.memory[self.vec[*reg as usize]] = v;
    }
    #[inline]
    pub fn change_reg_key(&mut self, reg: Register, k: MemKey) {
        self.vec[*reg as usize] = k;
    }
    #[inline]
    pub fn get_reg_key(&self, reg: Register) -> MemKey {
        self.vec[*reg as usize]
    }
    #[inline]
    pub fn get_reg<'a>(&self, reg: Register, vm: &'a mut Vm) -> &'a StoredValue {
        &vm.memory[self.vec[*reg as usize]]
    }
    #[inline]
    pub fn get_reg_mut<'a>(&self, reg: Register, vm: &'a mut Vm) -> &'a mut StoredValue {
        &mut vm.memory[self.vec[*reg as usize]]
    }
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
    pub fn consts(&self) -> &[Constant] {
        &self.bytecode().consts
    }
    #[inline]
    pub fn call_exprs(&self) -> &[CallExpr] {
        &self.bytecode().call_exprs
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

impl Vm {
    #[inline]
    pub fn store_value(&mut self, s: StoredValue) -> MemKey {
        self.memory.insert(s)
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

    pub fn run_func<F>(&mut self, info: RunInfo, cb: F) -> RuntimeResult<MemKey>
    where
        F: FnOnce(&mut Registers, &mut Vm),
    {
        let mut pos = 0;

        macro_rules! opcode {
            (Span) => {
                info.func().opcode_spans[pos]
            };
            (Area) => {
                opcode!(Span).into_area(info.src().clone())
            };
        }
        let mut regs = Registers::new(info.func().reg_count as usize, self, info);

        cb(&mut regs, self);

        macro_rules! bin_op {
            ($fn:ident => $a:ident, $b:ident, $to: ident) => {{
                let a = regs.get_reg_key($a);
                let b = regs.get_reg_key($b);
                let result = value_ops::$fn(self, a, b, opcode!(Span), info.src())?
                    .into_stored(opcode!(Area));
                regs.set_reg($to, result, self);
            }};
        }

        while pos < info.opcodes().len() {
            match info.opcodes()[pos] {
                Opcode::LoadConst(v, to) => {
                    regs.set_reg(
                        to,
                        Value::from_const(&info.consts()[*v as usize]).into_stored(opcode!(Area)),
                        self,
                    );
                }
                Opcode::LoadUnit(to) => {
                    regs.set_reg(to, Value::unit().into_stored(opcode!(Area)), self);
                }
                Opcode::CopyDeep(from, to) => {
                    let v = self.deep_clone_stored(regs.get_reg_key(from));
                    regs.set_reg(to, v, self);
                }
                Opcode::CopyShallow(from, to) => {
                    let v = regs.get_reg(from, self).clone();
                    regs.set_reg(to, v, self);
                }
                Opcode::CreateArray(reg, cap) => {
                    regs.set_reg(
                        reg,
                        Value::Array(Vec::with_capacity(cap as usize)).into_stored(opcode!(Area)),
                        self,
                    );
                }
                Opcode::CreateTuple(reg, cap) => {
                    regs.set_reg(
                        reg,
                        Value::Tuple(Vec::with_capacity(cap as usize)).into_stored(opcode!(Area)),
                        self,
                    );
                }
                Opcode::PushElem { from, arr } => {
                    let k = self.deep_clone_key(regs.get_reg_key(from));
                    if let Value::Array(v) | Value::Tuple(v) =
                        &mut regs.get_reg_mut(arr, self).value
                    {
                        v.push(k)
                    }
                }
                Opcode::Plus { a, b, to } => bin_op!(plus => a, b, to),
                Opcode::Minus { a, b, to } => bin_op!(minus => a, b, to),
                Opcode::Mult { a, b, to } => bin_op!(mult => a, b, to),
                Opcode::Div { a, b, to } => bin_op!(div => a, b, to),
                Opcode::Modulo { a, b, to } => bin_op!(modulo => a, b, to),
                Opcode::Pow { a, b, to } => bin_op!(pow => a, b, to),
                Opcode::Gt { a, b, to } => bin_op!(gt => a, b, to),
                Opcode::Gte { a, b, to } => bin_op!(gte => a, b, to),
                Opcode::Lt { a, b, to } => bin_op!(lt => a, b, to),
                Opcode::Lte { a, b, to } => bin_op!(lte => a, b, to),
                Opcode::Eq { a, b, to } => bin_op!(eq => a, b, to),
                Opcode::NotEq { a, b, to } => bin_op!(not_eq => a, b, to),
                Opcode::UnaryMinus { v, to } => todo!(),
                Opcode::UnaryNot { v, to } => todo!(),
                Opcode::Jump(v) => {
                    pos = *v as usize;
                    continue;
                }
                Opcode::JumpIfFalse(reg, to) => {
                    if !value_ops::as_bool(self, regs.get_reg_key(reg), opcode!(Span), info.src())?
                    {
                        pos = *to as usize;
                        continue;
                    }
                }
                Opcode::JumpIfTrue(reg, to) => {
                    if value_ops::as_bool(self, regs.get_reg_key(reg), opcode!(Span), info.src())? {
                        pos = *to as usize;
                        continue;
                    }
                }
                Opcode::Dbg(reg) => {
                    let v = regs.get_reg_key(reg);
                    println!("{}", self.value_str(v, true),)
                }
                o @ (Opcode::UnwrapArray { v, start, len }
                | Opcode::UnwrapTuple { v, start, len }) => {
                    let v = regs.get_reg_key(v);
                    match (
                        &self.memory[v].value,
                        matches!(o, Opcode::UnwrapTuple { .. }),
                    ) {
                        (Value::Array(arr), false) | (Value::Tuple(arr), true) => {
                            if arr.len() != len as usize {
                                return Err(RuntimeError::DestructureLenMismatch {
                                    expected: len as usize,
                                    found: arr.len(),
                                    val_area: self.memory[v].def_area.clone(),
                                    area: opcode!(Area),
                                });
                            }
                            let arr = arr.clone();
                            for i in 0..len {
                                let v = self.deep_clone_stored(arr[i as usize]);
                                regs.set_reg((*start + i).into(), v, self)
                            }
                        }
                        (_, t) => {
                            return Err(RuntimeError::TypeMismatch {
                                v: (
                                    self.memory[v].value.get_type(),
                                    self.memory[v].def_area.clone(),
                                ),
                                expected: if t {
                                    ValueType::Tuple
                                } else {
                                    ValueType::Array
                                },
                                area: opcode!(Area),
                            })
                        }
                    }
                }
                Opcode::CreateFunc {
                    id,
                    arg_amount,
                    reg,
                } => {
                    regs.set_reg(
                        reg,
                        Value::Function {
                            func: id,
                            param_types: vec![None; arg_amount as usize],
                            ret_type: None,
                            captured: info.bytecode().funcs[*id as usize]
                                .captured
                                .iter()
                                .map(|(from, _)| regs.get_reg_key(*from))
                                .collect(),
                        }
                        .into_stored(opcode!(Area)),
                        self,
                    );
                }
                Opcode::SetArgType { func, typ, arg } => {
                    let t = regs.get_reg_key(typ);
                    if let Value::Function { param_types, .. } =
                        &mut regs.get_reg_mut(func, self).value
                    {
                        param_types[arg as usize] = Some(t);
                    }
                }
                Opcode::SetReturnType { func, typ } => {
                    let t = regs.get_reg_key(typ);
                    if let Value::Function { ret_type, .. } =
                        &mut regs.get_reg_mut(func, self).value
                    {
                        *ret_type = Some(t);
                    }
                }
                Opcode::Call(v) => {
                    let call_expr = &info.call_exprs()[*v as usize];
                    let k = self.call_value(&regs, call_expr, opcode!(Area), info)?;
                    regs.set_reg(call_expr.ret, self.get(k).clone(), self);
                }
                Opcode::Return(reg) => return Ok(regs.get_reg_key(reg)),
                Opcode::Index { v, idx, out } => {
                    let v = regs.get_reg_key(v);
                    let idx = regs.get_reg_key(idx);
                    let result = value_ops::index(self, v, idx, opcode!(Span), info.src())?;
                    regs.change_reg_key(out, result);
                }
                Opcode::Import(id, out) => {
                    let new_info = RunInfo {
                        bytecode_idx: *id as usize,
                        func_idx: 0,
                        ..info
                    };

                    let v = self.run_func(new_info, dummy)?;
                    regs.change_reg_key(out, v);
                }
            }

            pos += 1;
        }

        Ok(self.store_value(
            Value::unit().into_stored(CodeSpan::ZEROSPAN.into_area(info.src().clone())),
        ))
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
        regs: &Registers,
        call_expr: &CallExpr,
        call_area: CodeArea,
        info: RunInfo,
    ) -> RuntimeResult<MemKey> {
        let base = regs.get_reg(call_expr.base, self);
        match &base.value {
            Value::Function {
                func,
                param_types,
                ret_type,
                captured,
            } => {
                if call_expr.args.len() != param_types.len() {
                    return Err(RuntimeError::IncorrectArgAmount {
                        expected: param_types.len(),
                        found: call_expr.args.len(),
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

                self.run_func(new_info, |new_regs: &mut Registers, vm: &mut Vm| {
                    for (idx, r) in call_expr.args.iter().enumerate() {
                        let v = vm.deep_clone_stored(regs.get_reg_key(*r));
                        new_regs.set_reg(idx.into(), v, vm);
                    }
                    for (k, (_, to)) in captured.iter().zip(new_info.func().captured.iter()) {
                        new_regs.change_reg_key(*to, *k);
                    }
                })
            }
            // Value::Type(t) => {
            //     if arg_amount != param_types.len() {
            //         return Err(RuntimeError::IncorrectArgAmount {
            //             expected: param_types.len(),
            //             found: arg_amount,
            //             val_area: base.def_area.clone(),
            //             area: call_area,
            //         });
            //     }
            // }
            _ => Err(RuntimeError::CannotCall {
                v: (base.value.get_type(), base.def_area.clone()),
                area: call_area,
            }),
        }
    }
}

pub fn dummy(_: &mut Registers, _: &mut Vm) {}
