use miette::SourceSpan;

#[derive(Eq, PartialEq, Debug, Clone, Default)]
pub struct Pos(usize);

#[derive(Eq, PartialEq, Debug, Clone, Default)]
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

    pub fn span_from_length(&self, length: usize) -> Span {
        Span::new(self.clone(), Some(Pos::new(self.offset() + length)))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_merge() {
        let s1 = Span::new(Pos::new(0), Some(Pos::new(10)));
        let s2 = Span::new(Pos::new(5), Some(Pos::new(15)));

        let merged = s1.merge(s2);

        assert_eq!(merged.start.offset(), 0);
        assert_eq!(merged.end.unwrap().offset(), 15);
    }

    #[test]
    fn test_span_merge_none() {
        let s1 = Span::new(Pos::new(0), Some(Pos::new(10)));
        let s2 = Span::new(Pos::new(5), None);

        let merged = s1.merge(s2);

        assert_eq!(merged.start.offset(), 0);
        assert_eq!(merged.end.unwrap().offset(), 10);
    }
}
