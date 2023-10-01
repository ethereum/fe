/// This module contains the logic for solving trait bounds.
use hir::hir_def::{ImplTrait, IngotId, Trait};
use rustc_hash::FxHashMap;

use crate::{ty::trait_lower::collect_trait_impls, HirAnalysisDb};

use super::{
    constraint::{collect_super_traits, ConstraintId},
    diagnostics::TraitConstraintDiag,
    ty_def::{Kind, Subst, TyId},
    unify::UnificationTable,
};

#[salsa::tracked(return_ref)]
pub(crate) fn ingot_trait_env(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitEnv {
    TraitEnv::new(db, ingot)
}

#[salsa::tracked(return_ref)]
pub(crate) fn trait_implementors(db: &dyn HirAnalysisDb, trait_: TraitInstId) -> Vec<Implementor> {
    let env = ingot_trait_env(db, trait_.ingot(db));
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let mut table = UnificationTable::new(db);
            impl_.generalize(db, &mut table);
            impl_.trait_(db).can_unify(db, trait_, &mut table)
        })
        .cloned()
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct TraitEnv {
    impls: FxHashMap<TraitDef, Vec<Implementor>>,
    ingot: IngotId,
}

impl TraitEnv {
    pub(super) fn new(db: &dyn HirAnalysisDb, ingot: IngotId) -> Self {
        let mut impls: FxHashMap<_, Vec<Implementor>> = FxHashMap::default();

        for impl_map in ingot
            .external_ingots(db.as_hir_db())
            .iter()
            .map(|(_, external)| &collect_trait_impls(db, *external).0)
            .chain(std::iter::once(&collect_trait_impls(db, ingot).0))
        {
            // `collect_trait_impls` ensure that there are no conflicting impls, so we can
            // just extend the map.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
        }

        Self { impls, ingot }
    }

    /// Returns all implementors of the given instantiated trait.
    pub(crate) fn implementors_for<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_: TraitInstId,
    ) -> &'db [Implementor] {
        trait_implementors(db, trait_)
    }
}

/// Represents an implementor of a trait.
#[salsa::interned]
pub(crate) struct Implementor {
    pub(crate) impl_def: ImplTrait,
    pub(crate) trait_: TraitInstId,
    pub(crate) ty: TyId,
    #[return_ref]
    pub(crate) params: Vec<TyId>,
}

impl Implementor {
    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &dyn HirAnalysisDb) -> TraitDef {
        self.trait_(db).def(db)
    }

    /// Generalizes the implementor by replacing all type parameters with fresh
    /// type variables.
    pub(super) fn generalize(self, db: &dyn HirAnalysisDb, table: &mut UnificationTable) -> Self {
        let mut subst = FxHashMap::default();
        for param in self.params(db) {
            let var = table.new_var(param.kind(db));
            subst.insert(*param, var);
        }

        let impl_def = self.impl_def(db);
        let trait_ = self.trait_(db).apply_subst(db, &mut subst);
        let ty = self.ty(db).apply_subst(db, &mut subst);
        let params = self
            .params(db)
            .iter()
            .map(|param| subst[param])
            .collect::<Vec<_>>();

        Implementor::new(db, impl_def, trait_, ty, params)
    }
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
pub(crate) struct TraitInstId {
    pub def: TraitDef,
    #[return_ref]
    pub substs: Vec<TyId>,

    ingot: IngotId,
}

impl TraitInstId {
    /// Apply substitutions to this trait.
    /// Which is to say, replace all type parameters with their corresponding
    /// type in the substitution.
    pub(super) fn apply_subst<S: Subst>(
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
            self.ingot(db),
        )
    }

    /// Returns `true` if this trait can be unified with the other trait.
    pub(super) fn can_unify(
        self,
        db: &dyn HirAnalysisDb,
        other: TraitInstId,
        table: &mut UnificationTable,
    ) -> bool {
        if self.def(db) != other.def(db) {
            return false;
        }

        for (&self_arg, &other_arg) in self.substs(db).iter().zip(other.substs(db)) {
            if !table.unify(self_arg, other_arg) {
                return false;
            }
        }

        true
    }
}

/// Represents a trait definition.
#[salsa::tracked]
pub(crate) struct TraitDef {
    pub trait_: Trait,
    #[return_ref]
    pub args: Vec<TyId>,
    /// We collects self type here to know the expected kind of implementor
    /// type in `Implementor` lowering phase.
    pub self_arg: TyId,
    // TODO: we need to collect associated method types here.
    // methods: Vec<FuncInst>
}

impl TraitDef {
    /// Returns the type kind that implementor type must have.
    pub(crate) fn expected_implementor_kind(self, db: &dyn HirAnalysisDb) -> &Kind {
        self.self_arg(db).kind(db)
    }

    /// Returns `ingot` in which this trait is defined.
    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        self.trait_(db).top_mod(hir_db).ingot(hir_db)
    }

    pub(super) fn super_traits<'db>(self, db: &'db dyn HirAnalysisDb) -> &'db [TraitInstId] {
        &collect_super_traits(db, self).0
    }
}
