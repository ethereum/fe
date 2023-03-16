use crate::hir_def::TypeId;

use super::{Body, IdentId, PathId};

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
    pub name: IdentId,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericParam {
    pub name: IdentId,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericArg {
    Type(TypeGenericArg),
    Const(ConstGenericArg),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericArg {
    pub ty: TypeId,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericArg {
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
    pub is_mut: bool,
    pub label: Option<FnParamLabel>,
    pub name: FnParamName,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: TypeId,
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
    Invalid,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeBound {
    /// The path to the trait.
    pub path: PathId,
    /// The type arguments of the trait.
    pub generic_args: Option<GenericArgListId>,
}
