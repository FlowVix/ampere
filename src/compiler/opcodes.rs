use super::proto::FuncID;

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
    CallExprID: u16;
    OpcodePos: u16;
    Register: u16;
    ImportID: u16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    LoadConst(ConstID, Register),

    LoadUnit(Register),

    CopyDeep(Register, Register),
    CopyShallow(Register, Register),

    Plus {
        a: Register,
        b: Register,
        to: Register,
    },
    Minus {
        a: Register,
        b: Register,
        to: Register,
    },
    Mult {
        a: Register,
        b: Register,
        to: Register,
    },
    Div {
        a: Register,
        b: Register,
        to: Register,
    },
    Modulo {
        a: Register,
        b: Register,
        to: Register,
    },
    Pow {
        a: Register,
        b: Register,
        to: Register,
    },

    Gt {
        a: Register,
        b: Register,
        to: Register,
    },
    Gte {
        a: Register,
        b: Register,
        to: Register,
    },
    Lt {
        a: Register,
        b: Register,
        to: Register,
    },
    Lte {
        a: Register,
        b: Register,
        to: Register,
    },
    Eq {
        a: Register,
        b: Register,
        to: Register,
    },
    NotEq {
        a: Register,
        b: Register,
        to: Register,
    },

    UnaryMinus {
        v: Register,
        to: Register,
    },
    UnaryNot {
        v: Register,
        to: Register,
    },

    Jump(OpcodePos),
    JumpIfFalse(Register, OpcodePos),
    JumpIfTrue(Register, OpcodePos),

    CreateArray(Register, u16),
    CreateTuple(Register, u16),
    PushElem {
        from: Register,
        arr: Register,
    },

    Dbg(Register),

    UnwrapArray {
        v: Register,
        start: Register,
        len: u16,
    },
    UnwrapTuple {
        v: Register,
        start: Register,
        len: u16,
    },

    CreateFunc {
        id: FuncID,
        arg_amount: u16,
        reg: Register,
    },
    SetArgType {
        func: Register,
        typ: Register,
        arg: u16,
    },
    SetReturnType {
        func: Register,
        typ: Register,
    },
    Call(CallExprID),

    Return(Register),

    Index {
        v: Register,
        idx: Register,
        out: Register,
    },

    Import(ImportID, Register),
}
