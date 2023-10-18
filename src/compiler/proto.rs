use crate::{
    make_ids,
    source::CodeSpan,
    util::{slabmap::SlabMap, unique_register::UniqueRegister},
};

use super::{bytecode::Constant, opcodes::Opcode};

make_ids! {
    BlockID: u16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpType {
    Start,
    End,
    StartIfFalse,
    EndIfFalse,
    StartIfTrue,
    EndIfTrue,
}

#[derive(Debug, Clone, Copy)]
pub enum ProtoOpcode {
    Raw(Opcode),
    Jump(BlockID, JumpType),
}

#[derive(Debug)]
pub enum BlockContent {
    Opcode(ProtoOpcode, CodeSpan),
    Block(BlockID),
}

pub struct ProtoFunc {
    pub code: BlockID,
    pub var_count: u16,
}

pub type Block = Vec<BlockContent>;

pub struct ProtoBytecode {
    pub consts: UniqueRegister<Constant>,
    pub functions: Vec<ProtoFunc>,
    pub blocks: SlabMap<BlockID, Block>,
}
