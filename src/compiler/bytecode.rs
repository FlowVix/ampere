use std::{hash::Hash, rc::Rc};

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

impl Eq for Constant {}
impl Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Constant::Int(v) => v.hash(state),
            Constant::Float(v) => v.to_bits().hash(state),
            Constant::String(v) => v.hash(state),
            Constant::Bool(v) => v.hash(state),
        }
    }
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
