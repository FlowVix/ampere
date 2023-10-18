use std::rc::Rc;

use crate::{
    source::{AmpereSource, CodeSpan},
    util::{ImmutStr, ImmutVec},
};

use super::opcodes::Opcode;

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int(i64),
    Float(f64),
    String(ImmutStr),
    Bool(bool),
}

pub struct Function {
    pub opcodes: ImmutVec<Opcode>,
    pub var_count: u16,
    pub opcode_spans: ImmutVec<CodeSpan>,
}

pub struct Bytecode {
    pub constants: ImmutVec<Constant>,
    pub funcs: ImmutVec<Function>,
    pub src: Rc<AmpereSource>,
}
