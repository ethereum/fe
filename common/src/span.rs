use serde::{
    Deserialize,
    Serialize,
};
use std::ops::{
    Add,
    AddAssign,
    Range,
};

/// An exclusive span of byte offsets in a source file.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone, Hash, Eq)]
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

impl Add for Span {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use std::cmp::{
            max,
            min,
        };
        Self {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }
}

impl<'a, T> Add<Option<&'a T>> for Span
where
    Span: Add<&'a T, Output = Self>,
{
    type Output = Self;

    fn add(self, other: Option<&'a T>) -> Self {
        if let Some(t) = other {
            self + t
        } else {
            self
        }
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        Range {
            start: span.start,
            end: span.end,
        }
    }
}
