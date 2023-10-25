use std::rc::Rc;

use ahash::AHashMap;

use crate::{
    compiler::{bytecode::Function, opcodes::OpcodePos},
    make_ids,
    source::{AmpereSource, CodeSpan},
    util::{slabmap::SlabMap, unique_register::UniqueRegister},
};

use super::{
    builder::CodeBuilder,
    bytecode::{Bytecode, CallExpr, Constant},
    opcodes::{Opcode, Register},
    CompileResult,
};

make_ids! {
    BlockID: u16;
    FuncID: u16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpType {
    Start,
    End,
    StartIfFalse(Register),
    EndIfFalse(Register),
    StartIfTrue(Register),
    EndIfTrue(Register),
}
impl JumpType {
    pub fn start(self) -> bool {
        match self {
            JumpType::Start | JumpType::StartIfFalse(_) | JumpType::StartIfTrue(_) => true,
            _ => false,
        }
    }
    pub fn opcode(self, pos: OpcodePos) -> Opcode {
        match self {
            JumpType::Start | JumpType::End => Opcode::Jump(pos),
            JumpType::StartIfFalse(reg) | JumpType::EndIfFalse(reg) => {
                Opcode::JumpIfFalse(reg, pos)
            }
            JumpType::StartIfTrue(reg) | JumpType::EndIfTrue(reg) => Opcode::JumpIfTrue(reg, pos),
        }
    }
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

#[derive(Debug)]
pub struct ProtoFunc {
    pub code: BlockID,
    pub reg_count: u16,
    pub captured: Vec<(Register, Register)>,
}

pub type Block = Vec<BlockContent>;

#[derive(Debug)]
pub struct ProtoBytecode {
    pub consts: UniqueRegister<Constant>,
    pub call_exprs: UniqueRegister<CallExpr>,

    pub functions: Vec<ProtoFunc>,
    pub blocks: SlabMap<BlockID, Block>,
}

impl ProtoBytecode {
    pub fn new_func<F>(&mut self, f: F, span: CodeSpan) -> CompileResult<FuncID>
    where
        F: FnOnce(&mut CodeBuilder) -> CompileResult<Vec<(Register, Register)>>,
    {
        let f_block = self.blocks.insert(Default::default());
        self.functions.push(ProtoFunc {
            code: f_block,
            reg_count: 0,
            captured: vec![],
        });
        let func = self.functions.len() - 1;
        let captured = f(&mut CodeBuilder {
            func,
            proto_bytecode: self,
            block: f_block,
        })?;
        self.functions[func].captured = captured;
        Ok(func.into())
    }

    pub fn build(mut self, src: &Rc<AmpereSource>) -> Bytecode {
        type BlockPos = (u16, u16);

        let consts = self.consts.make_vec();
        let call_exprs = self.call_exprs.make_vec();

        let mut funcs: Vec<Function> = vec![];

        for (func_id, func) in self.functions.iter().enumerate() {
            let mut block_positions = AHashMap::new();
            let mut code_len = 0;

            let mut opcodes: Vec<Opcode> = vec![];
            let mut opcode_spans: Vec<CodeSpan> = vec![];

            fn get_block_pos(
                code: &ProtoBytecode,
                block: BlockID,
                length: &mut u16,
                positions: &mut AHashMap<BlockID, BlockPos>,
            ) {
                let start = *length;
                for c in &code.blocks[block] {
                    match c {
                        BlockContent::Opcode(..) => {
                            *length += 1;
                        }
                        BlockContent::Block(b) => get_block_pos(code, *b, length, positions),
                    }
                }
                let end = *length;
                positions.insert(block, (start, end));
            }

            fn build_block(
                code: &ProtoBytecode,
                block: BlockID,
                opcodes: &mut Vec<Opcode>,
                opcode_spans: &mut Vec<CodeSpan>,
                positions: &AHashMap<BlockID, BlockPos>,
            ) {
                let get_jump_pos = |block: BlockID, start: bool| -> OpcodePos {
                    if start {
                        positions[&block].0
                    } else {
                        positions[&block].1
                    }
                    .into()
                };

                for content in &code.blocks[block] {
                    match content {
                        BlockContent::Opcode(o, span) => {
                            let opcode = match o {
                                ProtoOpcode::Raw(o) => *o,
                                ProtoOpcode::Jump(to, typ) => {
                                    let to = get_jump_pos(*to, typ.start());
                                    typ.opcode(to)
                                }
                            };
                            opcodes.push(opcode);
                            opcode_spans.push(*span);
                        }
                        BlockContent::Block(b) => {
                            build_block(code, *b, opcodes, opcode_spans, positions)
                        }
                    }
                }
            }

            get_block_pos(&self, func.code, &mut code_len, &mut block_positions);
            build_block(
                &self,
                func.code,
                &mut opcodes,
                &mut opcode_spans,
                &block_positions,
            );

            funcs.push(Function {
                opcodes: opcodes.into(),
                reg_count: func.reg_count,
                opcode_spans: opcode_spans.into(),
                captured: func.captured.clone().into(),
            })
        }

        Bytecode {
            consts: consts.into(),
            call_exprs: call_exprs.into(),
            funcs: funcs.into(),
            src: src.clone(),
        }
    }
}
