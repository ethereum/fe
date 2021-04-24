pub use fe_common::Span;
use serde::{Deserialize, Serialize};
use std::ops::Add;
use uuid::Uuid;

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq, Default)]
pub struct NodeId(Uuid);

impl NodeId {
    pub fn create() -> Self {
        Self(Uuid::new_v4())
    }
}

impl<T> Add<&Node<T>> for Span {
    type Output = Self;

    fn add(self, other: &Node<T>) -> Self {
        self + other.span
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
