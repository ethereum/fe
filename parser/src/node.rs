use serde::{
    Deserialize,
    Serialize,
};
use std::ops::{
    Add,
    AddAssign,
    Range,
};
use uuid::Uuid;

/// An exclusive span of byte offsets in a source file.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone, Hash, Eq)]
pub struct Span {
    /// A byte offset specifying the inclusive start of a span.
    pub start: usize,
    /// A byte offset specifying the exclusive end of a span.
    pub end: usize,
}

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq, Default)]
pub struct NodeId(Uuid);

impl NodeId {
    pub fn create() -> Self {
        Self(Uuid::new_v4())
    }
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

impl<T> Add<&Node<T>> for Span {
    type Output = Self;

    fn add(self, other: &Node<T>) -> Self {
        self + other.span
    }
}

impl<T> Add<Option<T>> for Span
where
    Span: Add<T, Output = Self>,
{
    type Output = Self;

    fn add(self, other: Option<T>) -> Self {
        if let Some(other) = other {
            self + other
        } else {
            self
        }
    }
}

impl<T> Add<&Option<Node<T>>> for Span {
    type Output = Self;

    fn add(self, other: &Option<Node<T>>) -> Self {
        if let Some(other) = other {
            self + other.span
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Node<T> {
    pub kind: T,
    #[serde(skip_serializing, skip_deserializing)]
    pub id: NodeId,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self {
            kind,
            id: NodeId::create(),
            span,
        }
    }

    /// Sets a new node ID.
    pub fn new_id(mut self) -> Self {
        self.id = NodeId::create();
        self
    }
}

impl<T> From<&Node<T>> for Span {
    fn from(node: &Node<T>) -> Self {
        node.span
    }
}

impl<T> From<&Box<Node<T>>> for Span {
    fn from(node: &Box<Node<T>>) -> Self {
        node.span
    }
}

impl<T> From<&Node<T>> for NodeId {
    fn from(node: &Node<T>) -> Self {
        node.id
    }
}

impl<T> From<&Box<Node<T>>> for NodeId {
    fn from(node: &Box<Node<T>>) -> Self {
        node.id
    }
}
