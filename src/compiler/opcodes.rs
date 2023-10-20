#[macro_export]
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
                derive_more::Deref,
                derive_more::DerefMut,
                Hash,
            )]
            pub struct $name($inner);

            impl From<usize> for $name {
                fn from(value: usize) -> Self {
                    Self(value as $inner)
                }
            }
            impl From<$inner> for $name {
                fn from(value: $inner) -> Self {
                    Self(value)
                }
            }

            impl From<$name> for usize {
                fn from(value: $name) -> Self {
                    value.0 as usize
                }
            }
            impl From<$name> for $inner {
                fn from(value: $name) -> Self {
                    value.0
                }
            }
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

    PopTop,
    PushUnit,

    Plus,
    Minus,
    Mult,
    Div,
    Modulo,
    Pow,

    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    NotEq,

    UnaryMinus,
    UnaryNot,

    Jump(OpcodePos),
    JumpIfFalse(OpcodePos),
    JumpIfTrue(OpcodePos),

    WrapArray(u16),
    WrapTuple(u16),

    Dbg,
}
