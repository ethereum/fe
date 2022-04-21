use cranelift_entity::{entity_impl, PrimaryMap};
use either::Either;

use super::{IdentId, ItemKind, PathId, TopLevelMod, TraitRef, TypeId, UseAlias};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeGraph {
    pub top_mod: TopLevelMod,
    pub scopes: PrimaryMap<LocalScopeId, LocalScope>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalScope {
    pub kind: ScopeKind,
    pub edges: Vec<ScopeEdge>,
}

impl LocalScope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            edges: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Item(ItemKind),
    GenericParam(usize),
    FnParam(usize),
    Field(usize),
    Variant(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopeEdge {
    pub dest: Either<TopLevelMod, LocalScopeId>,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum EdgeKind {
    Lex(LexEdge),
    Mod(ModEdge),
    GlobUse(GlobUseEdge),
    Use(UseEdge),
    Type(TypeEdge),
    Trait(TraitEdge),
    GenericParam(GenericParamEdge),
    Value(ValueEdge),
    Field(FieldEdge),
    Variant(VariantEdge),
    Super(SuperEdge),
    Ingot(IngotEdge),
    Self_(SelfEdge),
    SelfTy(SelfTyEdge),
    Anon(AnonEdge),
}

impl EdgeKind {
    pub fn lex() -> Self {
        EdgeKind::Lex(LexEdge())
    }

    pub fn mod_(ident: IdentId) -> Self {
        EdgeKind::Mod(ident.into())
    }

    pub fn glob_use(path: PathId, alias: UseAlias) -> Self {
        EdgeKind::GlobUse(GlobUseEdge { path, alias })
    }

    pub fn use_(path: PathId, alias: UseAlias) -> Self {
        EdgeKind::Use(UseEdge { path, alias })
    }

    pub fn type_(ident: IdentId) -> Self {
        EdgeKind::Type(ident.into())
    }

    pub fn trait_(ident: IdentId) -> Self {
        EdgeKind::Trait(ident.into())
    }

    pub fn generic_param(ident: IdentId) -> Self {
        EdgeKind::GenericParam(ident.into())
    }

    pub fn value(ident: IdentId) -> Self {
        EdgeKind::Value(ident.into())
    }

    pub fn field(ident: IdentId) -> Self {
        EdgeKind::Field(ident.into())
    }

    pub fn variant(ident: IdentId) -> Self {
        EdgeKind::Variant(ident.into())
    }

    pub fn super_() -> Self {
        EdgeKind::Super(SuperEdge())
    }

    pub fn ingot() -> Self {
        EdgeKind::Ingot(IngotEdge())
    }

    pub fn self_ty(ty: Either<TypeId, TraitRef>) -> Self {
        EdgeKind::SelfTy(ty.into())
    }

    pub fn self_() -> Self {
        EdgeKind::Self_(SelfEdge())
    }

    pub fn anon() -> Self {
        EdgeKind::Anon(AnonEdge())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LexEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct ModEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobUseEdge {
    /// `UsePathSegment` are lowered to a normal `Path`.
    path: PathId,
    alias: UseAlias,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UseEdge {
    /// `UsePathSegment` are lowered to a normal `Path`.
    path: PathId,
    alias: UseAlias,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct TypeEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct TraitEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct ValueEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct GenericParamEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct FieldEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct VariantEdge(IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SuperEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IngotEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct SelfTyEdge {
    ty: Either<TypeId, TraitRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct SelfEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AnonEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalScopeId(u32);
entity_impl!(LocalScopeId);
