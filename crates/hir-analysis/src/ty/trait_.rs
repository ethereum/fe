/// This module contains the logic for solving trait bounds.
use hir::hir_def::{ImplTrait, Trait};
use rustc_hash::FxHashMap;

use crate::HirAnalysisDb;

use super::ty::{Subst, TyId};

#[salsa::interned]
pub(crate) struct Implementor {
    pub(crate) impl_def: ImplTrait,
    pub(crate) trait_: TraitInstId,
    pub(crate) ty: TyId,
    #[return_ref]
    pub(crate) params: Vec<TyId>,
}

impl Implementor {
    pub(crate) fn trait_def(self, db: &dyn HirAnalysisDb) -> TraitDef {
        self.trait_(db).def(db)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) struct TraitImplTable {
    pub(crate) impls: FxHashMap<TraitDef, Vec<Implementor>>,
}

/// Represents an instantiated trait which is implemented to types.
#[salsa::interned]
pub(crate) struct TraitInstId {
    pub def: TraitDef,
    #[return_ref]
    pub substs: Vec<TyId>,
}

impl TraitInstId {
    pub(crate) fn apply_subst<S: Subst>(
        self,
        db: &dyn HirAnalysisDb,
        subst: &mut S,
    ) -> TraitInstId {
        TraitInstId::new(
            db,
            self.def(db),
            self.substs(db)
                .iter()
                .map(|ty| ty.apply_subst(db, subst))
                .collect(),
        )
    }
}

#[salsa::tracked]
pub(crate) struct TraitDef {
    pub trait_: Trait,
    #[return_ref]
    pub args: Vec<TyId>,
    pub self_arg: TyId,
    // TODO: we need to collect associated method types here.
    // methods: Vec<FuncInst>
}
