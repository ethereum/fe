pub mod attr;
pub mod body;
pub mod expr;
pub mod item;
pub mod params;
pub mod pat;
pub mod stmt;
pub mod types;
pub mod use_tree;

pub use attr::*;
pub use body::*;
pub use expr::*;
pub use item::*;
pub use params::*;
pub use pat::*;
pub use stmt::*;
pub use types::*;
pub use use_tree::*;

#[salsa::interned]
pub struct IdentId {
    data: String,
}

#[salsa::interned]
pub struct IntegerId {
    data: IntegerId,
}

#[salsa::interned]
pub struct StringId {
    data: String,
}

#[salsa::interned]
pub struct PathId {
    segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LitKind {
    Int(IntegerId),
    String(StringId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment {
    /// `Normal Path` segment.
    Ident(IdentId),
    /// `Self` segment.
    SelfTy,
    /// `self` segment.
    Self_,
}
