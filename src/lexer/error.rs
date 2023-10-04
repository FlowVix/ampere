#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerError {
    UnknownChar(char),
    UnterminatedString,
    // UnknownEscape(char),
}

impl LexerError {
    pub fn msg(&self) -> String {
        match self {
            LexerError::UnknownChar(c) => format!("Unknown character `{}`", c),
            LexerError::UnterminatedString => "Unterminated string".into(),
        }
    }
}
