//! This module contains the implementation of the constraint collection
//! algorithms.

use std::collections::BTreeSet;

use hir::hir_def::{
    scope_graph::ScopeId, GenericParam, GenericParamOwner, Impl, IngotId, ItemKind, TypeBound,
};
use salsa::function::Configuration;

use super::{
    constraint_solver::{is_goal_satisfiable, GoalSatisfiability},
    trait_def::{Implementor, TraitDef, TraitInstId},
    trait_lower::lower_trait_ref,
    ty_def::{AdtDef, FuncDef, HirFuncDefKind, TyBase, TyData, TyId},
    ty_lower::{collect_generic_params, lower_hir_ty, GenericParamOwnerId},
};
use crate::{
    ty::{
        binder::Binder,
        trait_lower::{lower_impl_trait, lower_trait},
        ty_def::{free_inference_keys, AdtRefId, TyVarSort},
        ty_lower::lower_adt,
        unify::InferenceKey,
    },
    HirAnalysisDb,
};

/// Returns a constraints list which is derived from the given type.
#[salsa::tracked]
pub(crate) fn ty_constraints(db: &dyn HirAnalysisDb, ty: TyId) -> ConstraintListId {
    let (base, args) = ty.decompose_ty_app(db);
    let (params, base_constraints) = match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => (adt.params(db), collect_adt_constraints(db, *adt)),
        TyData::TyBase(TyBase::Func(func_def)) => (
            func_def.params(db),
            collect_func_def_constraints(db, *func_def),
        ),
        _ => {
            return ConstraintListId::empty_list(db);
        }
    };

    let mut args = args.to_vec();

    // Generalize unbound type parameters.
    for &arg in params.iter().skip(args.len()) {
        let key = InferenceKey(args.len() as u32);
        let ty_var = TyId::ty_var(db, TyVarSort::General, arg.kind(db).clone(), key);
        args.push(ty_var);
    }

    let constraints = base_constraints.instantiate(db, &args);

    // If the constraint type contains unbound type parameters, we just ignore it.
    let mut new_constraints = BTreeSet::new();
    for &pred in constraints.predicates(db) {
        if free_inference_keys(db, pred.ty(db)).is_empty() {
            new_constraints.insert(pred);
        }
    }

    let ingot = constraints.ingot(db);
    ConstraintListId::new(db, new_constraints, ingot)
}

/// Collect super traits of the given trait.
/// The returned trait ref is bound by the given trait's generic parameters.
#[salsa::tracked(return_ref, recovery_fn = recover_collect_super_traits)]
pub(crate) fn collect_super_traits(
    db: &dyn HirAnalysisDb,
    trait_: TraitDef,
) -> Result<Binder<BTreeSet<TraitInstId>>, SuperTraitCycle> {
    let collector = SuperTraitCollector::new(db, trait_);
    let insts = collector.collect();

    let mut cycles = BTreeSet::new();
    // Check for cycles.
    for &inst in &insts {
        if let Err(err) = collect_super_traits(db, inst.def(db)) {
            cycles.extend(err.0.iter().copied());
        }
    }

    if cycles.is_empty() {
        Ok(Binder::bind(insts))
    } else {
        Err(SuperTraitCycle(cycles))
    }
}

/// Collect trait constraints that are specified by the given trait definition.
/// This constraints describes 1. the constraints about self type(i.e.,
/// implementor type), and 2. the generic parameter constraints.
#[salsa::tracked]
pub(crate) fn collect_trait_constraints(
    db: &dyn HirAnalysisDb,
    trait_: TraitDef,
) -> Binder<ConstraintListId> {
    let hir_trait = trait_.trait_(db);
    let collector = ConstraintCollector::new(db, GenericParamOwnerId::new(db, hir_trait.into()));

    Binder::bind(collector.collect())
}

/// Collect constraints that are specified by the given ADT definition.
#[salsa::tracked]
pub(crate) fn collect_adt_constraints(
    db: &dyn HirAnalysisDb,
    adt: AdtDef,
) -> Binder<ConstraintListId> {
    let Some(owner) = adt.as_generic_param_owner(db) else {
        return Binder::bind(ConstraintListId::empty_list(db));
    };
    let collector = ConstraintCollector::new(db, owner);

    Binder::bind(collector.collect())
}

#[salsa::tracked]
pub(crate) fn collect_impl_block_constraints(
    db: &dyn HirAnalysisDb,
    impl_: Impl,
) -> Binder<ConstraintListId> {
    let owner = GenericParamOwnerId::new(db, impl_.into());
    Binder::bind(ConstraintCollector::new(db, owner).collect())
}

/// Collect constraints that are specified by the given implementor(i.e., impl
/// trait).
#[salsa::tracked]
pub(crate) fn collect_implementor_constraints(
    db: &dyn HirAnalysisDb,
    implementor: Implementor,
) -> Binder<ConstraintListId> {
    let impl_trait = implementor.hir_impl_trait(db);
    let collector = ConstraintCollector::new(db, GenericParamOwnerId::new(db, impl_trait.into()));

    Binder::bind(collector.collect())
}

#[salsa::tracked]
pub(crate) fn collect_func_def_constraints(
    db: &dyn HirAnalysisDb,
    func: FuncDef,
) -> Binder<ConstraintListId> {
    let hir_func = match func.hir_def(db) {
        HirFuncDefKind::Func(func) => func,
        HirFuncDefKind::VariantCtor(enum_, _) => {
            let adt_ref = AdtRefId::new(db, enum_.into());
            let adt = lower_adt(db, adt_ref);
            return collect_adt_constraints(db, adt);
        }
    };

    let func_constraints = Binder::bind(
        ConstraintCollector::new(db, GenericParamOwnerId::new(db, hir_func.into())).collect(),
    );

    let parent_constraints = match hir_func.scope().parent_item(db.as_hir_db()) {
        Some(ItemKind::Trait(trait_)) => collect_trait_constraints(db, lower_trait(db, trait_)),

        Some(ItemKind::Impl(impl_)) => collect_impl_block_constraints(db, impl_),

        Some(ItemKind::ImplTrait(impl_trait)) => {
            let Some(implementor) = lower_impl_trait(db, impl_trait) else {
                return func_constraints;
            };
            collect_implementor_constraints(db, implementor.instantiate_identity())
        }

        _ => return func_constraints,
    };

    Binder::bind(
        func_constraints
            .instantiate_identity()
            .merge(db, parent_constraints.instantiate_identity()),
    )
}

/// Describes a predicate indicating `ty` implements `trait_`.
#[salsa::interned]
pub struct PredicateId {
    pub(super) ty: TyId,
    pub(super) trait_inst: TraitInstId,
}

impl PredicateId {
    pub fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        let ty = self.ty(db);
        let trait_ = self.trait_inst(db);
        format!("{}: {}", ty.pretty_print(db), trait_.pretty_print(db))
    }
}

/// The list of predicates.
#[salsa::interned]
pub(crate) struct PredicateListId {
    #[return_ref]
    pub(super) predicates: BTreeSet<PredicateId>,
    pub(super) ingot: IngotId,
}

impl PredicateListId {
    pub(super) fn remove(self, db: &dyn HirAnalysisDb, pred: PredicateId) -> Self {
        if !self.contains(db, pred) {
            return self;
        };

        let mut predicates = self.predicates(db).clone();
        predicates.remove(&pred);
        PredicateListId::new(db, predicates, self.ingot(db))
    }

    pub(super) fn merge(self, db: &dyn HirAnalysisDb, other: Self) -> Self {
        let mut predicates = self.predicates(db).clone();
        predicates.extend(other.predicates(db));
        PredicateListId::new(db, predicates, self.ingot(db))
    }

    pub(super) fn empty_list(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, BTreeSet::new(), IngotId::dummy())
    }

    pub(super) fn contains(self, db: &dyn HirAnalysisDb, pred: PredicateId) -> bool {
        self.predicates(db).contains(&pred)
    }
}

pub(super) type AssumptionListId = PredicateListId;
pub(super) type ConstraintListId = PredicateListId;

pub(crate) fn recover_collect_super_traits(
    _db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    _trait_: TraitDef,
) -> Result<Binder<BTreeSet<TraitInstId>>, SuperTraitCycle> {
    let mut trait_cycle = BTreeSet::new();
    for key in cycle.participant_keys() {
        trait_cycle.insert(collect_super_traits::key_from_id(key.key_index()));
    }

    Err(SuperTraitCycle(trait_cycle))
}

struct SuperTraitCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef,
    super_traits: BTreeSet<TraitInstId>,
    scope: ScopeId,
}

impl<'db> SuperTraitCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: TraitDef) -> Self {
        Self {
            db,
            trait_,
            super_traits: BTreeSet::default(),
            scope: trait_.trait_(db).scope(),
        }
    }

    fn collect(mut self) -> BTreeSet<TraitInstId> {
        let hir_trait = self.trait_.trait_(self.db);
        let hir_db = self.db.as_hir_db();
        let self_param = self.trait_.self_param(self.db);

        for &super_ in hir_trait.super_traits(hir_db).iter() {
            if let Ok(inst) = lower_trait_ref(self.db, self_param, super_, self.scope) {
                self.super_traits.insert(inst);
            }
        }

        for pred in hir_trait.where_clause(hir_db).data(hir_db) {
            if pred
                .ty
                .to_opt()
                .map(|ty| ty.is_self_ty(hir_db))
                .unwrap_or_default()
            {
                for bound in &pred.bounds {
                    if let TypeBound::Trait(bound) = bound {
                        if let Ok(inst) = lower_trait_ref(self.db, self_param, *bound, self.scope) {
                            self.super_traits.insert(inst);
                        }
                    }
                }
            }
        }

        self.super_traits
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub(crate) struct SuperTraitCycle(BTreeSet<TraitDef>);
impl SuperTraitCycle {
    pub fn contains(&self, def: TraitDef) -> bool {
        self.0.contains(&def)
    }
}

struct ConstraintCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwnerId,
    predicates: BTreeSet<PredicateId>,
}

impl<'db> ConstraintCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwnerId) -> Self {
        Self {
            db,
            owner,

            predicates: BTreeSet::new(),
        }
    }

    fn collect(mut self) -> ConstraintListId {
        self.collect_constraints_from_generic_params();
        self.collect_constraints_from_where_clause();
        self.simplify()
    }

    fn simplify(&mut self) -> PredicateListId {
        let ingot = self.owner.ingot(self.db);

        // Collect super traits from the trait definition and add them to the predicate
        // list.
        if let GenericParamOwner::Trait(trait_) = self.owner.data(self.db) {
            let trait_def = lower_trait(self.db, trait_);
            let self_param = trait_def.self_param(self.db);
            let super_traits = trait_def
                .super_traits(self.db)
                .clone()
                .instantiate(self.db, trait_def.params(self.db));

            for &super_trait in super_traits.iter() {
                let inst = super_trait;
                self.push_predicate(self_param, inst)
            }
        }

        let mut simplified = PredicateListId::new(self.db, self.predicates.clone(), ingot);

        for goal in std::mem::take(&mut self.predicates).into_iter() {
            let temp_predicates = simplified.remove(self.db, goal);
            if self.can_remove(temp_predicates, goal) {
                simplified = temp_predicates;
            }
        }

        simplified
    }

    fn can_remove(&mut self, predicates: PredicateListId, goal: PredicateId) -> bool {
        let goal_ty = goal.ty(self.db);

        if !goal_ty.contains_ty_param(self.db) {
            return true;
        }

        match is_goal_satisfiable(self.db, goal, predicates) {
            GoalSatisfiability::Satisfied | GoalSatisfiability::InfiniteRecursion(_) => true,
            GoalSatisfiability::NotSatisfied(_) => false,
        }
    }

    fn push_predicate(&mut self, ty: TyId, trait_inst: TraitInstId) {
        let pred = PredicateId::new(self.db, ty, trait_inst);
        self.predicates.insert(pred);
    }

    fn collect_constraints_from_where_clause(&mut self) {
        let Some(where_clause) = self.owner.where_clause(self.db) else {
            return;
        };

        for hir_pred in where_clause.data(self.db.as_hir_db()) {
            let Some(hir_ty) = hir_pred.ty.to_opt() else {
                continue;
            };

            let ty = lower_hir_ty(self.db, hir_ty, self.owner.scope(self.db));

            // We don't need to collect super traits, please refer to
            // [`collect_super_traits`] function for details.
            if ty.is_invalid(self.db) || ty.is_trait_self(self.db) {
                return;
            }

            self.add_bounds(ty, &hir_pred.bounds);
        }
    }

    fn collect_constraints_from_generic_params(&mut self) {
        let param_set = collect_generic_params(self.db, self.owner);
        let param_list = self.owner.params(self.db);

        for (i, hir_param) in param_list.data(self.db.as_hir_db()).iter().enumerate() {
            let GenericParam::Type(hir_param) = hir_param else {
                continue;
            };

            let ty = param_set.param_by_original_idx(self.db, i).unwrap();
            let bounds = &hir_param.bounds;
            self.add_bounds(ty, bounds)
        }
    }

    fn add_bounds(&mut self, bound_ty: TyId, bounds: &[TypeBound]) {
        for bound in bounds {
            let TypeBound::Trait(trait_ref) = bound else {
                continue;
            };

            let Ok(trait_inst) =
                lower_trait_ref(self.db, bound_ty, *trait_ref, self.owner.scope(self.db))
            else {
                continue;
            };

            self.push_predicate(bound_ty, trait_inst);
        }
    }
}
