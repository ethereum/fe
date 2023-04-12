use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    segments: Vec<Partial<PathSegment>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment {
    /// `ingot`.
    Ingot,
    /// `super`.
    Super,
    /// `Self` segment.
    SelfTy,
    /// `self` segment.
    Self_,
    Ident(IdentId),
}
