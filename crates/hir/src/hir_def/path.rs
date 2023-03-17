use crate::hir_def::MaybeInvalid;

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    segments: Vec<MaybeInvalid<PathSegment>>,
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
