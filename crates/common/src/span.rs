use crate::files::SourceFileId;
use serde::{Deserialize, Serialize};
use std::cmp;
use std::fmt::{Debug, Formatter};
use std::ops::{Add, AddAssign, Range};

/// An exclusive span of byte offsets in a source file.
#[derive(Serialize, Deserialize, PartialEq, Copy, Clone, Hash, Eq)]
pub struct Span {
    #[serde(skip_serializing)]
    pub file_id: SourceFileId,
    /// A byte offset specifying the inclusive start of a span.
    pub start: usize,
    /// A byte offset specifying the exclusive end of a span.
    pub end: usize,
}

impl Span {
    pub fn new(file_id: SourceFileId, start: usize, end: usize) -> Self {
        Span {
            file_id,
            start: cmp::min(start, end),
            end: cmp::max(start, end),
        }
    }

    pub fn zero(file_id: SourceFileId) -> Self {
        Span {
            file_id,
            start: 0,
            end: 0,
        }
    }

    pub fn dummy() -> Self {
        Self {
            file_id: SourceFileId::dummy_file(),
            start: usize::MAX,
            end: usize::MAX,
        }
    }

    pub fn is_dummy(&self) -> bool {
        self == &Self::dummy()
    }

    pub fn from_pair<S, E>(start_elem: S, end_elem: E) -> Self
    where
        S: Into<Span>,
        E: Into<Span>,
    {
        let start_span: Span = start_elem.into();
        let end_span: Span = end_elem.into();

        let file_id = if start_span.file_id == end_span.file_id {
            start_span.file_id
        } else {
            panic!("file ids are not equal")
        };

        Self {
            file_id,
            start: start_span.start,
            end: end_span.end,
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", Range::from(*self))
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Add for Span {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use std::cmp::{max, min};

        let file_id = if self.file_id == other.file_id {
            self.file_id
        } else {
            panic!("file ids are not equal")
        };

        Self {
            file_id,
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }
}

impl Add<Option<Span>> for Span {
    type Output = Self;

    fn add(self, other: Option<Span>) -> Self {
        if let Some(other) = other {
            self + other
        } else {
            self
        }
    }
}

impl<'a, T> Add<Option<&'a T>> for Span
where
    Span: Add<&'a T, Output = Self>,
{
    type Output = Self;

    fn add(self, other: Option<&'a T>) -> Self {
        if let Some(other) = other {
            self + other
        } else {
            self
        }
    }
}

impl<'a, T> Add<&'a T> for Span
where
    T: Spanned,
{
    type Output = Self;

    fn add(self, other: &'a T) -> Self {
        self + other.span()
    }
}

impl<T> AddAssign<T> for Span
where
    Span: Add<T, Output = Self>,
{
    fn add_assign(&mut self, other: T) {
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
