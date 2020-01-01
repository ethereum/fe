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
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    #[inline]
    pub fn from_pair<S, E>(start_elem: S, end_elem: E) -> Self
    where
        S: Into<Span>,
        E: Into<Span>,
    {
        let start_span: Span = start_elem.into();
        let end_span: Span = end_elem.into();

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

impl<T> From<&Spanned<T>> for Span {
    fn from(spanned: &Spanned<T>) -> Span {
        spanned.span
    }
}
