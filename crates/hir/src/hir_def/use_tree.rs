use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct UseTreeId {
    /// The base path of the use tree.
    /// `Foo::Foo2` in `Foo::Foo2::{Bar::*, Baz::{x, y}}`
    ///
    /// NOTE: If the tree root is started with `{}`, then the `path` is `None`.
    #[return_ref]
    pub path: Vec<Partial<UsePathSegment>>,

    /// The subtree of the use tree.
    ///
    /// `Bar::*` and `Baz::{x, y}` in `Foo::Foo2::{Bar::*, Baz::{x, y}}`.
    #[return_ref]
    pub subtree: Vec<UseTreeId>,

    //// The alias of this use tree.
    /// `Bar` in `Foo as Bar;`
    #[return_ref]
    pub alias: Option<Partial<UseAlias>>,
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
