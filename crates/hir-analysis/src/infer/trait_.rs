/// This module contains the logic for solving trait bounds.
use hir::hir_def::{Func, Trait};
use rustc_hash::{FxHashMap, FxHashSet};

use super::ty::TyId;

/// `Ty` implements `Trait` with the given type arguments.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub trait_: TraitInstId,
    pub ty: TyId,
    pub trait_args: Vec<TyId>,
}

/// Represents an each type which implements a trait.
/// Whenever `predicates` are satisfied, `impl_` is satisfied.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Implementor {
    predicates: Vec<Predicate>,
    impl_: Predicate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitImplTable {
    pub impls: FxHashMap<TraitInstId, Implementor>,
    pub method_table: FxHashMap<TyId, FxHashSet<Func>>,
}

/// Represents an instantiated trait which is implemented to types.
#[salsa::interned]
pub struct TraitInstId {
    pub trait_: TraitDef,
    pub substs: Vec<TyId>,
    pub super_traits: Vec<TraitInstId>,
}

#[salsa::tracked]
pub struct TraitDef {
    pub trait_: Trait,
    pub args: TyId,
    pub predicates: Vec<Predicate>,
    pub super_traits: Vec<TraitDef>,
}
