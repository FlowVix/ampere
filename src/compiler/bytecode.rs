use std::{hash::Hash, rc::Rc};

use colored::Colorize;
use itertools::Itertools;

use crate::{
    source::{AmpereSource, CodeSpan},
    util::{ImmutStr, ImmutVec},
};

use super::opcodes::{Opcode, Register};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub args: ImmutVec<Register>,
    pub base: Register,
    pub ret: Register,
}

pub struct Function {
    pub opcodes: ImmutVec<Opcode>,
    pub reg_count: u16,
    pub captured: ImmutVec<(Register, Register)>,
    pub opcode_spans: ImmutVec<CodeSpan>,
}

pub struct Bytecode {
    pub consts: ImmutVec<Constant>,
    pub call_exprs: ImmutVec<CallExpr>,

    pub funcs: ImmutVec<Function>,
    pub src: Rc<AmpereSource>,
}

impl Bytecode {
    pub fn display(&self) {
        println!(
            "Constants: {}\n",
            self.consts
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
                "- reg count: ".dimmed(),
                func.reg_count.to_string().bright_blue()
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
