use miette::SourceSpan;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Pos(usize);

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Span {
    pub start: Pos,
    pub end: Option<Pos>,
}

impl Pos {
    pub fn new(offset: usize) -> Self {
        Pos(offset)
    }

    pub fn offset(&self) -> usize {
        self.0
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos(0)
    }
}

impl From<usize> for Pos {
    fn from(offset: usize) -> Self {
        Pos(offset)
    }
}

impl Span {
    pub fn new(start: Pos, end: Option<Pos>) -> Self {
        Span { start, end }
    }

    pub fn start(&self) -> Pos {
        self.start.clone()
    }

    pub fn end(&self) -> Option<Pos> {
        self.end.clone()
    }

    pub fn merge(&self, s2: Span) -> Self {
        let start = self.start();
        let end = match (&self.end, &s2.end) {
            (Some(p1), None) => Some(p1.clone()),
            (_, Some(p2)) => Some(p2.clone()),
            (None, None) => None,
        };

        Self { start, end }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span {
            start: Pos::default(),
            end: None,
        }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Self {
        Span::new(pos.clone(), Some(pos))
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        let start = span.start.offset();
        let length = if let Some(end) = span.end {
            end.offset() - span.start.offset() + 1
        } else {
            0
        };

        Self::new(start.into(), length.into())
    }
}
