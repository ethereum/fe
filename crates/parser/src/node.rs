pub use fe_common::{Span, Spanned};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq, Default, PartialOrd, Ord)]
pub struct NodeId(u32);

impl NodeId {
    pub fn create() -> Self {
        static NEXT_ID: AtomicU32 = AtomicU32::new(0);
        Self(NEXT_ID.fetch_add(1, Ordering::Relaxed))
    }

    pub fn dummy() -> Self {
        Self(u32::MAX)
    }

    pub fn is_dummy(self) -> bool {
        self == Self::dummy()
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Node<T> {
    pub kind: T,
    #[serde(skip_serializing, skip_deserializing)]
    pub id: NodeId,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        let id = NodeId::create();
        Self { kind, id, span }
    }
}

impl<T> Spanned for Node<T> {
    fn span(&self) -> Span {
        self.span
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
