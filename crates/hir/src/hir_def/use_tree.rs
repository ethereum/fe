use super::{IdentId, PathId};

#[salsa::interned]
pub struct UseTreeId {
    pub path: Option<UsePath>,
    pub subtree: Vec<UseTreeId>,
    pub alias: Option<UseTreeAlias>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UsePath {
    Path(PathId),
    /// `*`.
    Glob,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UseTreeAlias {
    Ident(IdentId),
    Underscore,
}
