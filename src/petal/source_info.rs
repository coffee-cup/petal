#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Pos(usize);

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Span(Pos, Pos);

impl Pos {
    pub fn new(offset: usize) -> Self {
        Pos(offset)
    }
}

impl From<usize> for Pos {
    fn from(offset: usize) -> Self {
        Pos(offset)
    }
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span(start, end)
    }

    pub fn start(&self) -> Pos {
        self.0.clone()
    }

    pub fn end(&self) -> Pos {
        self.1.clone()
    }

    pub fn merge(&self, s2: Span) -> Self {
        Span(self.start(), s2.end())
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Self {
        Span(pos.clone(), pos)
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}
