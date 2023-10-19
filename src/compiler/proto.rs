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
    bytecode::{Bytecode, Constant},
    opcodes::Opcode,
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
    StartIfFalse,
    EndIfFalse,
    StartIfTrue,
    EndIfTrue,
}
impl JumpType {
    pub fn start(self) -> bool {
        match self {
            JumpType::Start | JumpType::StartIfFalse | JumpType::StartIfTrue => true,
            _ => false,
        }
    }
    pub fn opcode_f(self) -> fn(OpcodePos) -> Opcode {
        match self {
            JumpType::Start | JumpType::End => Opcode::Jump,
            JumpType::StartIfFalse | JumpType::EndIfFalse => Opcode::JumpIfFalse,
            JumpType::StartIfTrue | JumpType::EndIfTrue => Opcode::JumpIfTrue,
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
    pub var_count: u16,
}

pub type Block = Vec<BlockContent>;

#[derive(Debug)]
pub struct ProtoBytecode {
    pub consts: UniqueRegister<Constant>,
    pub functions: Vec<ProtoFunc>,
    pub blocks: SlabMap<BlockID, Block>,
}

impl ProtoBytecode {
    pub fn new_func<F>(&mut self, f: F, span: CodeSpan) -> CompileResult<FuncID>
    where
        F: FnOnce(&mut CodeBuilder) -> CompileResult<()>,
    {
        let f_block = self.blocks.insert(Default::default());
        self.functions.push(ProtoFunc {
            code: f_block,
            var_count: 0,
        });
        let func = self.functions.len() - 1;
        f(&mut CodeBuilder {
            func,
            proto_bytecode: self,
            block: f_block,
        })?;
        Ok(func.into())
    }

    pub fn build(mut self, src: &Rc<AmpereSource>) -> Bytecode {
        type BlockPos = (u16, u16);

        let constants = self.consts.make_vec();

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
                                    let f = typ.opcode_f();
                                    f(to)
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
                var_count: func.var_count,
                opcode_spans: opcode_spans.into(),
            })
        }

        Bytecode {
            constants: constants.into(),
            funcs: funcs.into(),
            src: src.clone(),
        }
    }
}
