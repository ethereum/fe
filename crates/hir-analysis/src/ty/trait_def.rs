//! This module contains all trait related types definitions.

use common::indexmap::{IndexMap, IndexSet};
use hir::{
    hir_def::{IdentId, ImplTrait, IngotId, Trait},
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::Canonical,
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    func_def::FuncDef,
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        check_trait_inst_wf,
        constraint::{collect_implementor_constraints, collect_super_traits},
        PredicateListId, WellFormedness,
    },
    ty_def::{Kind, TyId},
    ty_lower::GenericParamTypeSet,
    unify::UnificationTable,
};
use crate::{
    ty::{trait_lower::collect_trait_impls, trait_resolution::constraint::super_trait_cycle},
    HirAnalysisDb,
};

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref)]
pub(crate) fn ingot_trait_env<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all [`Implementor`] for the given trait inst.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<Binder<Implementor<'db>>> {
    let mut table = UnificationTable::new(db);
    let trait_ = trait_.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();
            let impl_ = table.instantiate_with_fresh_vars(**impl_);
            let is_ok = table.unify(impl_.trait_(db), trait_).is_ok();
            table.rollback_to(snapshot);
            is_ok
        })
        .cloned()
        .collect()
}

/// Returns all [`Implementor`] for the given `ty`.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<Binder<Implementor<'db>>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    if ty.has_invalid(db) {
        return vec![];
    }

    let mut cands = vec![];
    for (key, insts) in env.ty_to_implementors.iter() {
        let snapshot = table.snapshot();
        let key = table.instantiate_with_fresh_vars(*key);
        if table.unify(key, ty.base_ty(db)).is_ok() {
            cands.push(insts);
        }

        table.rollback_to(snapshot);
    }

    cands
        .into_iter()
        .flatten()
        .copied()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let impl_ = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(impl_.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, Update)]
pub(crate) struct TraitEnv<'db> {
    pub(super) impls: FxHashMap<TraitDef<'db>, Vec<Binder<Implementor<'db>>>>,
    hir_to_implementor: FxHashMap<ImplTrait<'db>, Binder<Implementor<'db>>>,

    /// This maintains a mapping from the base type to the implementors.
    ty_to_implementors: FxHashMap<Binder<TyId<'db>>, Vec<Binder<Implementor<'db>>>>,

    ingot: IngotId<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: IngotId<'db>) -> Self {
        let mut impls: FxHashMap<_, Vec<Binder<Implementor>>> = FxHashMap::default();
        let mut hir_to_implementor: FxHashMap<ImplTrait, Binder<Implementor>> =
            FxHashMap::default();
        let mut ty_to_implementors: FxHashMap<Binder<TyId>, Vec<Binder<Implementor>>> =
            FxHashMap::default();

        for impl_map in ingot
            .external_ingots(db)
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // `collect_trait_impls` ensures that there are no conflicting impls, so we can
            // just extend the map.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());

                hir_to_implementor.extend(implementors.iter().map(|implementor| {
                    (implementor.skip_binder().hir_impl_trait(db), *implementor)
                }));

                for implementor in implementors {
                    ty_to_implementors
                        .entry(Binder::bind(
                            implementor.instantiate_identity().self_ty(db).base_ty(db),
                        ))
                        .or_default()
                        .push(*implementor);
                }
            }
        }

        Self {
            impls,
            hir_to_implementor,
            ty_to_implementors,
            ingot,
        }
    }

    /// Returns the corresponding implementor of the given `impl Trait` type.
    pub(crate) fn map_impl_trait(&self, trait_ref: ImplTrait) -> Option<Binder<Implementor>> {
        self.hir_to_implementor.get(&trait_ref).copied()
    }
}

/// Represents an implementor of a trait, which can be thought of as a lowered
/// `impl Trait`.
#[salsa::interned]
#[derive(Debug)]
pub(crate) struct Implementor<'db> {
    /// The trait that this implementor implements.
    pub(crate) trait_: TraitInstId<'db>,

    /// The type parameters of this implementor.
    #[return_ref]
    pub(crate) params: Vec<TyId<'db>>,

    #[return_ref]
    pub(crate) types: IndexMap<IdentId<'db>, TyId<'db>>,

    /// The original hir.
    pub(crate) hir_impl_trait: ImplTrait<'db>,
}

impl<'db> Implementor<'db> {
    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &'db dyn HirAnalysisDb) -> TraitDef<'db> {
        self.trait_(db).def(db)
    }

    pub(crate) fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.params(db)
    }

    /// The self type of the impl trait.
    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(super) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        collect_implementor_constraints(db, self).instantiate(db, self.params(db))
    }

    pub(super) fn methods(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexMap<IdentId<'db>, FuncDef<'db>> {
        collect_implementor_methods(db, self)
    }
}

/// Returns `true` if the given two implementor conflicts.
pub(super) fn does_impl_trait_conflict(
    db: &dyn HirAnalysisDb,
    a: Binder<Implementor>,
    b: Binder<Implementor>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let a = table.instantiate_with_fresh_vars(a);
    let b = table.instantiate_with_fresh_vars(b);

    table.unify(a, b).is_ok()
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
#[derive(Debug)]
pub struct TraitInstId<'db> {
    pub def: TraitDef<'db>,
    #[return_ref]
    pub args: Vec<TyId<'db>>,
}

impl<'db> TraitInstId<'db> {
    pub fn pretty_print(self, db: &dyn HirAnalysisDb, as_pred: bool) -> String {
        if as_pred {
            let inst = self.pretty_print(db, false);
            let self_ty = self.self_ty(db);
            format! {"{}: {}", self_ty.pretty_print(db), inst}
        } else {
            let mut s = self.def(db).name(db).unwrap_or("<unknown>").to_string();

            let mut args = self.args(db).iter().map(|ty| ty.pretty_print(db));
            // Skip the first type parameter since it's the implementor type.
            args.next();

            if let Some(first) = args.next() {
                s.push('<');
                s.push_str(first);
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg);
                }
                s.push('>');
            }

            s
        }
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }

    pub(super) fn ingot(self, db: &'db dyn HirAnalysisDb) -> IngotId<'db> {
        self.def(db).ingot(db)
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        ingot: IngotId<'db>,
        assumptions: PredicateListId<'db>,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_trait_inst_wf(db, ingot, self, assumptions)
        {
            Some(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            )
        } else {
            None
        }
    }
}

/// Represents a trait definition.
#[salsa::tracked]
#[derive(Debug)]
pub struct TraitDef<'db> {
    pub trait_: Trait<'db>,
    #[return_ref]
    pub(crate) param_set: GenericParamTypeSet<'db>,
    #[return_ref]
    pub methods: IndexMap<IdentId<'db>, TraitMethod<'db>>,
}

impl<'db> TraitDef<'db> {
    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    pub fn self_param(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.param_set(db).trait_self(db).unwrap()
    }

    pub fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, salsa::Update)]
pub struct TraitMethod<'db>(pub FuncDef<'db>);

impl TraitMethod<'_> {
    pub fn has_default_impl(self, db: &dyn HirAnalysisDb) -> bool {
        self.0.hir_func_def(db).unwrap().body(db).is_some()
    }
}

impl<'db> TraitDef<'db> {
    /// Returns the type kind that implementor type must have.
    pub(crate) fn expected_implementor_kind(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        self.self_param(db).kind(db)
    }

    /// Returns `ingot` in which this trait is defined.
    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> IngotId<'db> {
        let hir_db = db;
        self.trait_(db).top_mod(hir_db).ingot(hir_db)
    }

    pub fn super_traits(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexSet<Binder<TraitInstId<'db>>> {
        use std::sync::OnceLock;
        static EMPTY: OnceLock<IndexSet<Binder<TraitInstId>>> = OnceLock::new();

        if super_trait_cycle(db, self).is_some() {
            EMPTY.get_or_init(IndexSet::new)
        } else {
            collect_super_traits(db, self)
        }
    }

    fn name(self, db: &'db dyn HirAnalysisDb) -> Option<&'db str> {
        self.trait_(db)
            .name(db)
            .to_opt()
            .map(|name| name.data(db).as_str())
    }
}
