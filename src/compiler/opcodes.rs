macro_rules! make_ids {
    (
        $(
            $name:ident: $inner:ty;
        )*
    ) => {
        $(
            #[derive(
                Debug,
                Clone,
                Copy,
                PartialEq,
                Eq,
                PartialOrd,
                Ord,
                derive_more::Into,
                derive_more::From,
                derive_more::Deref,
                derive_more::DerefMut,
            )]
            pub struct $name($inner);
        )*
    };
}

make_ids! {
    ConstID: u16;
    VarID: u16;
    OpcodePos: u16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    LoadConst(ConstID),

    SetVar(VarID),
    LoadVar(VarID),

    Plus,
    Minus,
    Mult,
    Div,
    Modulo,

    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    NEq,

    Jump(OpcodePos),
    JumpIfFalse(OpcodePos),
    JumpIfTrue(OpcodePos),
}
