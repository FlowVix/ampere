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
    pub fn read(&self) -> String {
        match self {
            AmpereSource::File(path) => {
                let mut file = File::open(path).unwrap();
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap();
                contents
            }
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
        write!(
            f,
            "<{} @ {:?}>",
            match &*self.src {
                AmpereSource::File(path) => path.to_str().unwrap(),
            },
            self.span
        )
    }
}
