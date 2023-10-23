use ahash::AHashMap;
use lasso::Spur;

use crate::{make_ids, source::CodeSpan};

use super::{opcodes::VarID, proto::BlockID};

make_ids! {
    ScopeID: u32;
}

#[derive(Debug, Clone, Copy)]
pub struct VarData {
    pub def_span: CodeSpan,
    pub id: VarID,
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeType {
    File,
    Normal,
    Loop(BlockID),
    FuncBody,
}

pub struct Scope {
    pub vars: AHashMap<Spur, VarData>,
    pub parent: Option<ScopeID>,
    pub typ: ScopeType,
}
