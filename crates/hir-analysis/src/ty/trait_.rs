/// This module contains the logic for solving trait bounds.
use hir::hir_def::{ImplTrait, IngotId, Trait};
use rustc_hash::FxHashMap;

use crate::HirAnalysisDb;

use super::{
    diagnostics::TraitSatisfactionDiag,
    ty_def::{Kind, Subst, TyId},
};

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

    pub(crate) fn substs(self, db: &dyn HirAnalysisDb) -> &Vec<TyId> {
        self.trait_(db).substs(db)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub(crate) struct TraitImplTable {
    pub(crate) impls: FxHashMap<TraitDef, Vec<Implementor>>,
}

impl TraitImplTable {
    pub(crate) fn insert(&mut self, db: &dyn HirAnalysisDb, implementor: Implementor) {
        self.impls
            .entry(implementor.trait_def(db))
            .or_default()
            .push(implementor);
    }

    pub(crate) fn get(&self, trait_def: TraitDef) -> Option<&Vec<Implementor>> {
        self.impls.get(&trait_def)
    }
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

impl TraitDef {
    pub(crate) fn expected_implementor_kind(self, db: &dyn HirAnalysisDb) -> &Kind {
        self.self_arg(db).kind(db)
    }

    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        self.trait_(db).top_mod(hir_db).ingot(hir_db)
    }
}
