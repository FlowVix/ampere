use crate::source::CodeSpan;

use super::{
    bytecode::Constant,
    opcodes::{Opcode, VarID},
    proto::{Block, BlockContent, BlockID, JumpType, ProtoBytecode, ProtoFunc, ProtoOpcode},
};

pub struct CodeBuilder<'a> {
    pub proto_bytecode: &'a mut ProtoBytecode,
    pub func: usize,
    pub block: BlockID,
}

impl<'a> CodeBuilder<'a> {
    fn current_func(&mut self) -> &mut ProtoFunc {
        &mut self.proto_bytecode.functions[self.func]
    }
    fn current_block(&mut self) -> &mut Block {
        &mut self.proto_bytecode.blocks[self.block]
    }
    pub fn new_var(&mut self) -> VarID {
        let v = self.current_func().var_count.into();
        self.current_func().var_count += 1;
        v
    }

    pub fn load_const(&mut self, c: Constant, span: CodeSpan) {
        let id = self.proto_bytecode.consts.insert(c).into();
        self.push_opcode(ProtoOpcode::Raw(Opcode::LoadConst(id)), span)
    }

    fn push_opcode(&mut self, opcode: ProtoOpcode, span: CodeSpan) {
        self.current_block()
            .push(BlockContent::Opcode(opcode, span))
    }
    pub fn push_raw_opcode(&mut self, opcode: Opcode, span: CodeSpan) {
        self.push_opcode(ProtoOpcode::Raw(opcode), span)
    }
    pub fn push_jump(&mut self, block: Option<BlockID>, jump_type: JumpType, span: CodeSpan) {
        let block = block.unwrap_or(self.block);

        self.push_opcode(ProtoOpcode::Jump(block, jump_type), span);
    }
}
