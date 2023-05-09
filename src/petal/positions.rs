#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }

    pub fn merge(&self, s2: Span) -> Self {
        Span {
            start: self.start.clone(),
            end: s2.end,
        }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Self {
        Span {
            start: pos.clone(),
            end: pos,
        }
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}
