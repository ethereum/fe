use crate::hir_def::TypeId;

use super::{Body, IdentId, MaybeInvalid, PathId};

#[salsa::interned]
pub struct GenericArgListId {
    #[return_ref]
    pub args: Vec<GenericArg>,
}

#[salsa::interned]
pub struct GenericParamListId {
    #[return_ref]
    pub params: Vec<GenericParam>,
}

#[salsa::interned]
pub struct FnParamListId {
    #[return_ref]
    args: Vec<FnParam>,
}

#[salsa::interned]
pub struct WhereClauseId {
    #[return_ref]
    pub predicates: Vec<WherePredicate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericParam {
    Type(TypeGenericParam),
    Const(ConstGenericParam),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericParam {
    pub name: MaybeInvalid<IdentId>,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericParam {
    pub name: MaybeInvalid<IdentId>,
    pub ty: MaybeInvalid<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericArg {
    Type(TypeGenericArg),
    Const(ConstGenericArg),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericArg {
    pub ty: MaybeInvalid<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericArg {
    pub body: MaybeInvalid<Body>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
    pub is_mut: bool,
    pub label: Option<FnParamLabel>,
    pub name: MaybeInvalid<FnParamName>,
    pub ty: MaybeInvalid<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: MaybeInvalid<TypeId>,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FnParamLabel {
    Ident(IdentId),
    Underscore,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FnParamName {
    /// `self` parameter.
    Self_,
    Ident(IdentId),
    Underscore,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeBound {
    /// The path to the trait.
    pub path: MaybeInvalid<PathId>,
    /// The type arguments of the trait.
    pub generic_args: Option<GenericArgListId>,
}
