use ahash::AHashMap;
use lasso::Spur;

use crate::{make_ids, source::CodeSpan};

use super::opcodes::VarID;

make_ids! {
    ScopeID: u32;
}

#[derive(Debug, Clone, Copy)]
pub struct VarData {
    pub def_span: CodeSpan,
    pub id: VarID,
}

pub struct Scope {
    pub vars: AHashMap<Spur, VarData>,
    pub parent: Option<ScopeID>,
}
