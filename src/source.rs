use std::{fmt::Debug, fs::File, io::Read, ops::Range, path::PathBuf, rc::Rc};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    pub const ZEROSPAN: Self = CodeSpan { start: 0, end: 0 };

    pub fn extended(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }

    pub fn into_area(self, src: Rc<AmpereSource>) -> CodeArea {
        CodeArea { span: self, src }
    }
}

impl Debug for CodeSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range<usize>> for CodeSpan {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
impl From<CodeSpan> for Range<usize> {
    fn from(value: CodeSpan) -> Self {
        value.start..value.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AmpereSource {
    File(PathBuf),
}

impl AmpereSource {
    pub fn read(&self) -> Option<String> {
        match self {
            AmpereSource::File(path) => std::fs::read_to_string(path).ok(),
        }
    }
    pub fn name(&self) -> String {
        match self {
            AmpereSource::File(path) => path.to_str().unwrap().into(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CodeArea {
    pub span: CodeSpan,
    pub src: Rc<AmpereSource>,
}

impl Debug for CodeArea {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} @ {:?}>", self.src.name(), self.span)
    }
}
