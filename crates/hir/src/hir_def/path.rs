use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    segments: Vec<Partial<PathSegment>>,
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
