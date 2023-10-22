use std::{hash::Hash, rc::Rc};

use colored::Colorize;
use itertools::Itertools;

use crate::{
    source::{AmpereSource, CodeSpan},
    util::{ImmutStr, ImmutVec},
};

use super::opcodes::{Opcode, VarID};

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
    pub captured: ImmutVec<(VarID, VarID)>,
    pub opcode_spans: ImmutVec<CodeSpan>,
}

pub struct Bytecode {
    pub constants: ImmutVec<Constant>,
    pub funcs: ImmutVec<Function>,
    pub src: Rc<AmpereSource>,
}

impl Bytecode {
    pub fn display(&self) {
        println!(
            "Constants: {}\n",
            self.constants
                .iter()
                .map(|v| format!("{:?}", v).bright_green())
                .join(", ")
        );

        for (i, func) in self.funcs.iter().enumerate() {
            println!(
                "{}",
                format!("============ Function {} ============", i)
                    .bright_yellow()
                    .bold()
            );
            println!(
                "{}{}",
                "- var count: ".dimmed(),
                func.var_count.to_string().bright_blue()
            );
            println!(
                "{}{}",
                "- captured: ".dimmed(),
                format!("{:?}", func.captured).bright_blue()
            );

            for (i, opcode) in func.opcodes.iter().enumerate() {
                println!("{}: {:?}", i, opcode);
            }
        }
    }
}
