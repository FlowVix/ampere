use crate::source::CodeSpan;

use super::{
    bytecode::Constant,
    opcodes::{Opcode, Register},
    proto::{
        Block, BlockContent, BlockID, FuncID, JumpType, ProtoBytecode, ProtoFunc, ProtoOpcode,
    },
    CompileResult,
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
    pub fn next_reg(&mut self) -> Register {
        let v = self.current_func().reg_count.into();
        self.current_func().reg_count += 1;
        v
    }
    pub fn peek_next_reg(&mut self) -> Register {
        self.current_func().reg_count.into()
    }

    pub fn new_func<F>(&mut self, f: F, span: CodeSpan) -> CompileResult<FuncID>
    where
        F: FnOnce(&mut CodeBuilder) -> CompileResult<Vec<(Register, Register)>>,
    {
        self.proto_bytecode.new_func(f, span)
    }

    pub fn in_block<F: FnOnce(&mut CodeBuilder) -> CompileResult<()>>(
        &mut self,
        id: BlockID,
        f: F,
    ) -> CompileResult<()> {
        let mut builder = CodeBuilder {
            block: id,
            func: self.func,
            proto_bytecode: self.proto_bytecode,
        };

        f(&mut builder)
    }

    pub fn new_block<F: FnOnce(&mut CodeBuilder) -> CompileResult<()>>(
        &mut self,
        f: F,
    ) -> CompileResult<BlockID> {
        let f_block = self.proto_bytecode.blocks.insert(Default::default());

        self.current_block().push(BlockContent::Block(f_block));

        self.in_block(f_block, f)?;

        Ok(f_block)
    }

    pub fn load_const(&mut self, c: Constant, to: Register, span: CodeSpan) {
        let id = self.proto_bytecode.consts.insert(c).into();
        self.push_opcode(ProtoOpcode::Raw(Opcode::LoadConst(id, to)), span)
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
