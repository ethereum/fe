use crate::hir_def::TypeId;

use super::{Expr, IdentId, PathId};

#[salsa::interned]
pub struct GenericArgListId {
    #[return_ref]
    pub args: Vec<GenericArg>,
}

#[salsa::interned]
pub struct WhereClauseId {
    #[return_ref]
    pub predicates: Vec<WherePredicate>,
}

#[salsa::interned]
pub struct FnParamListId {
    #[return_ref]
    args: Vec<FnParam>,
}

#[salsa::interned]
pub struct GenericParamListId {
    #[return_ref]
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
    pub name: IdentId,
    pub label: Option<IdentId>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicateId {
    pub ty: TypeId,
    pub bound: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Type(TypeGenericArg),
    Const(ConstGenericArg),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericArg {
    pub path: PathId,
    pub bounds: Vec<TypeBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericArg {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeBound {
    pub path: PathId,
    pub generic_args: Vec<GenericArg>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate {
    pub ty: TypeId,
    pub bound: Vec<TypeBound>,
}
