use serde::{
    Deserialize,
    Serialize,
};

/// An exclusive span of byte offsets in a source file.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub struct Span {
    /// A byte offset specifying the inclusive start of a span.
    pub start: usize,
    /// A byte offset specifying the exclusive end of a span.
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

impl From<(&Span, &Span)> for Span {
    fn from(spans: (&Span, &Span)) -> Self {
        let (start_span, end_span) = spans;

        Self {
            start: start_span.start,
            end: end_span.end,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
