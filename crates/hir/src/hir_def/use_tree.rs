use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct UsePathId {
    #[return_ref]
    pub segments: Vec<Partial<UsePathSegment>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UsePathSegment {
    Ident(IdentId),
    /// `*`.
    Glob,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UseAlias {
    Ident(IdentId),
    Underscore,
}
