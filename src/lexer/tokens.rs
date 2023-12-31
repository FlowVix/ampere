macro_rules! tokens {
    (
        $(
            $tok:ident: $name:literal,
        )*
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Token {
            $($tok,)*
        }

        impl Token {
            pub fn to_str(self) -> &'static str {
                match self {
                    $(
                        Self::$tok => $name,
                    )*
                }
            }
        }
    };
}

tokens! {
    Int: "int literal",
    Float: "float literal",
    String: "string literal",

    True: "true",
    False: "false",

    Identifier: "identifier",

    Plus: "+",
    Minus: "-",
    Mult: "*",
    Div: "/",
    Mod: "%",
    Pow: "**",
    PlusEq: "+=",
    MinusEq: "-=",
    MultEq: "*=",
    DivEq: "/=",
    ModEq: "%=",
    PowEq: "**=",

    Assign: "=",

    Eq: "==",
    NotEq: "!=",
    Gt: ">",
    Gte: ">=",
    Lt: "<",
    Lte: "<=",

    Not: "!",

    OpenParen: "(",
    ClosedParen: ")",
    OpenSqBracket: "[",
    ClosedSqBracket: "]",
    OpenBracket: "{",
    ClosedBracket: "}",


    If: "if",
    Else: "else",
    While: "while",
    For: "for",

    Return: "return",
    Break: "break",
    Continue: "continue",

    Import: "import",

    Let: "let",

    Dbg: "dbg",

    Colon: ":",
    Semicolon: ";",
    Period: ".",
    Comma: ",",
    Arrow: "->",
    FatArrow: "=>",

    Eof: "end of file",
}
