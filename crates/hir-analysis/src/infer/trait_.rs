/// This module contains the logic for solving trait bounds.
use hir::hir_def::{Func, Trait};
use rustc_hash::{FxHashMap, FxHashSet};

use super::ty::{TyId, TyVar};

/// `Ty` implements `Trait` with the given type arguments.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Predicate {
    pub trait_: TraitInstId,
    pub ty: TyId,
    pub trait_args: Vec<TyId>,
}

/// T is satisfied under the given predicates.
/// i.e., `predicates => T`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Qualified<T> {
    predicates: Vec<Predicate>,
    t: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitImplTable {
    pub impls: FxHashMap<TraitInstId, Qualified<Predicate>>,
    pub method_table: FxHashMap<TyId, FxHashSet<Func>>,
}

/// Represents an instantiated trait which is implemented to types.
#[salsa::interned]
pub struct TraitInstId {
    pub trait_: Trait,
    pub args: Vec<TyId>,
    pub super_traits: Vec<TraitInstId>,
}

pub struct TraitDef {
    pub trait_: Trait,
    pub args: Vec<Qualified<TyVar>>,
    pub super_traits: Vec<TraitDef>,
}
