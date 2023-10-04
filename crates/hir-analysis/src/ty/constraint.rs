use std::collections::{BTreeMap, BTreeSet};

use hir::{
    hir_def::{
        self, scope_graph::ScopeId, GenericParamListId, GenericParamOwner, IngotId, Trait,
        TraitRef, TypeId, TypeKind, WhereClauseId, WherePredicate,
    },
    visitor::prelude::*,
};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::function::Configuration;

use crate::{
    ty::{
        trait_::TraitEnv,
        trait_lower::lower_trait,
        unify::{InferenceKey, UnificationTable},
    },
    HirAnalysisDb,
};

use super::{
    constraint_solver::{is_goal_satisfiable, GoalSatisfiability},
    diagnostics::{TraitConstraintDiag, TraitLowerDiag, TyDiagCollection},
    trait_::{TraitDef, TraitInstId},
    trait_lower::lower_trait_ref,
    ty_def::{AdtDef, AdtRef, AdtRefId, InvalidCause, Subst, TyConcrete, TyData, TyId},
    ty_lower::{collect_generic_params, lower_hir_ty, GenericParamOwnerId, GenericParamTypeSet},
};

#[salsa::tracked]
pub(crate) fn ty_constraints(
    db: &dyn HirAnalysisDb,
    ty: TyId,
) -> (AssumptionListId, ConstraintListId) {
    assert!(ty.free_inference_keys(db).is_empty());

    let (base, args) = ty.decompose_ty_app(db);
    let TyData::TyCon(TyConcrete::Adt(adt)) = ty.data(db) else {
        return (
            AssumptionListId::empty_list(db),
            ConstraintListId::empty_list(db),
        );
    };

    let mut subst = FxHashMap::default();
    let mut arg_idx = 0;
    for (&param, arg) in adt.params(db).iter().zip(args) {
        subst.insert(param, arg);
        arg_idx += 1;
    }

    /// Generalize unbound type parameters.
    for &arg in adt.params(db).iter().skip(arg_idx) {
        let key = InferenceKey(arg_idx as u32);
        let ty_var = TyId::ty_var(db, arg.kind(db).clone(), key);
        subst.insert(arg, ty_var);
        arg_idx += 1;
    }

    /// Substitute type parameters.
    let constraint = collect_adt_constraints(db, adt).apply_subst(db, &mut subst);

    /// If the predicate type is a type variable, collect it as an assumption
    /// and remove it from the constraint.
    let mut new_assumptions = BTreeSet::new();
    let mut new_constraints = BTreeSet::new();
    for &pred in constraint.predicates(db) {
        if pred.ty(db).is_ty_var(db) {
            new_assumptions.insert(pred);
        } else {
            new_constraints.insert(pred);
        }
    }

    let ingot = constraint.ingot(db);
    (
        AssumptionListId::new(db, new_assumptions, ingot),
        ConstraintListId::new(db, new_constraints, ingot),
    )
}

/// Collects super traits, and verify there are no cyclic in
/// the super traits relationship.
///
/// This method is implemented independently from [`collect_trait_constraints`]
/// method because
/// 1. the cycle check should be performed before collecting other constraints
///    to make sure the constraint simplification terminates.
/// 2. `collect_trait_constraints` function needs to care about another cycle
///    which is caused by constraints simplification.
/// 3. we want to emit better error messages for cyclic super traits.
///
/// NOTE: This methods returns all super traits without any simplification.
#[salsa::tracked(return_ref, recovery_fn = recover_collect_super_traits)]
pub(crate) fn collect_super_traits(
    db: &dyn HirAnalysisDb,
    trait_: TraitDef,
) -> (Vec<TraitInstId>, Vec<TyDiagCollection>) {
    let mut collector = SuperTraitCollector::new(db, trait_, FxHashSet::default());
    let (insts, diags) = collector.finalize();

    // Check for cycles.
    for inst in &insts {
        collect_super_traits(db, inst.def(db));
    }

    (insts, diags)
}

/// Returns a list of super trait instances of the given trait instance.
#[salsa::tracked(return_ref)]
pub(crate) fn super_trait_insts(
    db: &dyn HirAnalysisDb,
    trait_inst: TraitInstId,
) -> Vec<TraitInstId> {
    let trait_def = trait_inst.def(db);
    let (super_traits, _) = collect_super_traits(db, trait_def);
    let mut subst = trait_inst.subst_table(db);

    super_traits
        .iter()
        .map(|trait_| trait_.apply_subst(db, &mut subst))
        .collect()
}

#[salsa::tracked]
pub(crate) fn collect_trait_constraints(
    db: &dyn HirAnalysisDb,
    trait_: TraitDef,
) -> ConstraintListId {
    let hir_trait = trait_.trait_(db);
    let mut collector = ConstraintCollector::new(
        db,
        hir_trait.scope(),
        GenericParamOwnerId::new(db, hir_trait.into()),
    );

    let mut ctxt = VisitorCtxt::with_trait(db.as_hir_db(), hir_trait);
    collector.visit_trait(&mut ctxt, hir_trait);

    // TODO: accumulates errors.
    collector.finalize().0
}

#[salsa::tracked]
pub(crate) fn collect_adt_constraints(db: &dyn HirAnalysisDb, adt: AdtDef) -> ConstraintListId {
    let ingot = adt.ingot(db);
    let Some(owner) = adt.as_generic_param_owner(db) else {
        return ConstraintListId::empty_list(db);
    };
    let mut collector = ConstraintCollector::new(db, adt.scope(db), owner);
    match adt.adt_ref(db).data(db) {
        AdtRef::Contract(_) => return ConstraintListId::empty_list(db),
        AdtRef::Enum(enum_) => {
            let mut ctxt = VisitorCtxt::with_enum(db.as_hir_db(), enum_);
            collector.visit_enum(&mut ctxt, enum_);
        }
        AdtRef::Struct(struct_) => {
            let mut ctxt = VisitorCtxt::with_struct(db.as_hir_db(), struct_);
            collector.visit_struct(&mut ctxt, struct_);
        }
    }

    collector.finalize().0
}

/// Returns a list of assumptions obtained by the given assumptions by looking
/// up super traits.
#[salsa::tracked(return_ref)]
pub(crate) fn compute_super_assumptions(
    db: &dyn HirAnalysisDb,
    assumptions: AssumptionListId,
) -> AssumptionListId {
    let ingot = assumptions.ingot(db);
    let trait_env = TraitEnv::new(db, ingot);
    let mut super_assumptions = BTreeSet::new();

    for pred in assumptions.predicates(db) {
        let ty = pred.ty(db);
        let inst = pred.trait_inst(db);
        for &super_inst in super_trait_insts(db, inst) {
            let super_pred = PredicateId::new(db, ty, super_inst);
            super_assumptions.insert(super_pred);
        }
    }

    AssumptionListId::new(db, super_assumptions, ingot)
}

#[salsa::interned]
pub struct PredicateId {
    pub(super) ty: TyId,
    pub(super) trait_inst: TraitInstId,
}

impl PredicateId {
    pub fn apply_subst<S: Subst>(self, db: &dyn HirAnalysisDb, subst: &mut S) -> Self {
        let ty = self.ty(db).apply_subst(db, subst);
        let trait_ = self.trait_inst(db).apply_subst(db, subst);
        Self::new(db, ty, trait_)
    }
}

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

    pub(super) fn empty_list(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, BTreeSet::new(), IngotId::dummy())
    }

    pub(super) fn contains(self, db: &dyn HirAnalysisDb, pred: PredicateId) -> bool {
        self.predicates(db).contains(&pred)
    }

    pub(super) fn apply_subst<S: Subst>(self, db: &dyn HirAnalysisDb, s: &mut S) -> Self {
        let predicates = self
            .predicates(db)
            .iter()
            .map(|pred| pred.apply_subst(db, s))
            .collect();

        Self::new(db, predicates, self.ingot(db))
    }
}

pub(super) type AssumptionListId = PredicateListId;
pub(super) type ConstraintListId = PredicateListId;

pub(crate) fn recover_collect_super_traits(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    trait_: TraitDef,
) -> (Vec<TraitInstId>, Vec<TyDiagCollection>) {
    let participants: FxHashSet<_> = cycle
        .participant_keys()
        .map(|key| {
            let trait_ = collect_super_traits::key_from_id(key.key_index());
            trait_.trait_(db)
        })
        .collect();

    let mut collector = SuperTraitCollector::new(db, trait_, participants);
    collector.finalize()
}

struct SuperTraitCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef,
    super_traits: Vec<TraitInstId>,
    diags: Vec<TyDiagCollection>,
    cycle: FxHashSet<Trait>,
    scope: ScopeId,
}

impl<'db> SuperTraitCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: TraitDef, cycle: FxHashSet<Trait>) -> Self {
        Self {
            db,
            trait_,
            super_traits: vec![],
            diags: vec![],
            cycle,
            scope: trait_.trait_(db).scope(),
        }
    }

    fn finalize(mut self) -> (Vec<TraitInstId>, Vec<TyDiagCollection>) {
        let hir_trait = self.trait_.trait_(self.db);
        let mut visitor_ctxt = VisitorCtxt::with_trait(self.db.as_hir_db(), hir_trait);
        self.visit_trait(&mut visitor_ctxt, hir_trait);

        (self.super_traits, self.diags)
    }
}

impl<'db> Visitor for SuperTraitCollector<'db> {
    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRef,
    ) {
        let span = ctxt.span().unwrap();
        let (trait_inst, diags) = lower_trait_ref(self.db, trait_ref, span, self.scope);
        if !diags.is_empty() {
            self.diags.extend(diags);
            return;
        }

        let Some(trait_inst) = trait_inst else {
            return;
        };

        if self
            .cycle
            .contains(&trait_inst.def(self.db).trait_(self.db))
        {
            let span = ctxt.span().unwrap().into();
            self.diags
                .push(TraitLowerDiag::CyclicSuperTraits(span).into());
            return;
        }

        self.super_traits.push(trait_inst);
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &WherePredicate,
    ) {
        // We just want to check super traits, so we don't care about other type
        // constraints.
        if pred
            .ty
            .to_opt()
            .map(|ty| ty.is_self_ty(self.db.as_hir_db()))
            .unwrap_or_default()
        {
            walk_where_predicate(self, ctxt, pred);
        }
    }

    fn visit_item(&mut self, _: &mut VisitorCtxt<'_, LazyItemSpan>, _: hir::hir_def::ItemKind) {
        // We don't want to visit nested items in the trait.
    }
}

struct ConstraintCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection>,

    owner: GenericParamOwnerId,

    predicates: BTreeSet<PredicateId>,
    predicate_span_map: BTreeMap<PredicateId, DynLazySpan>,

    current_ty: TyId,
}

impl<'db> ConstraintCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId, owner: GenericParamOwnerId) -> Self {
        Self {
            db,
            diags: vec![],

            owner,

            predicates: BTreeSet::new(),
            predicate_span_map: BTreeMap::new(),

            current_ty: TyId::invalid(db, InvalidCause::Other),
        }
    }

    fn finalize(mut self) -> (ConstraintListId, Vec<TyDiagCollection>) {
        let simplified = self.simplify();
        (simplified, self.diags)
    }

    fn simplify(&mut self) -> PredicateListId {
        let ingot = self.owner.ingot(self.db);

        // Collect super traits from the trait definition and add them to the predicate
        // list.
        if let GenericParamOwner::Trait(trait_) = self.owner.data(self.db) {
            let trait_def = lower_trait(self.db, trait_);
            let self_param = trait_def.self_param(self.db);
            for &inst in &collect_super_traits(self.db, trait_def).0 {
                self.push_predicate(self_param, inst, DynLazySpan::invalid());
            }
        }

        let mut simplified = PredicateListId::new(self.db, self.predicates.clone(), ingot);

        for goal in std::mem::take(&mut self.predicates).into_iter() {
            let temp_predicates = simplified.remove(self.db, goal);
            if self.can_remove(temp_predicates, goal) {
                simplified = temp_predicates;
            }
        }

        // TODO: Report if types and trait insts are not satisfy the constraints.
        simplified
    }

    fn can_remove(&mut self, predicates: PredicateListId, goal: PredicateId) -> bool {
        let goal_ty = goal.ty(self.db);
        let must_satisfy = goal_ty.contains_ty_param(self.db);

        match is_goal_satisfiable(self.db, goal, predicates) {
            GoalSatisfiability::Satisfied => true,
            GoalSatisfiability::NonSatisfied if must_satisfy => {
                let span = &self.predicate_span_map[&goal];
                let diag =
                    TraitConstraintDiag::trait_bound_not_satisfied(self.db, span.clone(), goal);
                self.diags.push(diag.into());
                true
            }
            GoalSatisfiability::NonSatisfied => false,
            GoalSatisfiability::InfiniteRecursion => {
                /// TODO: Report error.
                true
            }
        }
    }

    fn push_predicate(&mut self, ty: TyId, trait_inst: TraitInstId, span: DynLazySpan) {
        let pred = PredicateId::new(self.db, ty, trait_inst);
        self.predicates.insert(pred);
        self.predicate_span_map.insert(pred, span);
    }
}

impl<'db> Visitor for ConstraintCollector<'db> {
    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &WherePredicate,
    ) {
        let Some(hir_ty) = pred.ty.to_opt() else {
            return;
        };

        let ty = lower_hir_ty(self.db, hir_ty, self.owner.scope(self.db));

        // We don't need to collect super traits, please refer to
        // `collect_super_traits` // function for details.
        if ty.is_invalid(self.db) || ty.is_trait_self(self.db) {
            return;
        }

        self.current_ty = ty;
        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRef,
    ) {
        let (trait_inst, diag) = lower_trait_ref(
            self.db,
            trait_ref,
            ctxt.span().unwrap(),
            self.owner.scope(self.db),
        );

        let Some(trait_ref) = trait_inst else {
            return;
        };

        self.push_predicate(self.current_ty, trait_ref, ctxt.span().unwrap().into());
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyGenericParamSpan>,
        param: &hir_def::GenericParam,
    ) {
        let ScopeId::GenericParam(_, param_idx) = ctxt.scope() else {
            unreachable!()
        };
        self.current_ty = collect_generic_params(self.db, self.owner).params[param_idx];

        walk_generic_param(self, ctxt, param);
    }

    fn visit_item(&mut self, _: &mut VisitorCtxt<'_, LazyItemSpan>, _: hir::hir_def::ItemKind) {
        // We don't want to visit nested items.
    }
}
