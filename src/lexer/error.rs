#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerError {
    UnknownChar(char),
    UnterminatedString,
    // UnknownEscape(char),
}
