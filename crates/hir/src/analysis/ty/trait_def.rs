//! This module contains all trait related types definitions.

use crate::{
    analysis::ty::{
        method_cmp::{compare_impl_method, method_param_correspondence},
        trait_lower::{
            ImplSelfKey, collect_trait_impls, complete_impl_trait, complete_selected_impl,
            lower_impl_trait_header,
        },
        trait_resolution::{GoalSatisfiability, PredicateListId, Selection},
    },
    hir_def::{
        Body, Contract, Func, GenericParamOwner, HirIngot, IdentId, ImplTrait, ModeMarker, Trait,
        params::FuncParamMode, scope_graph::ScopeId,
    },
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::{Ingot, IngotKind},
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    canonical::Canonical,
    closure::CallableShape,
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::{TyFoldable, TyFolder},
    normalize::normalize_ty,
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        TraitSolveCx, constraint::collect_candidate_constraints, is_goal_satisfiable,
        normalize_trait_inst_preserving_validity,
    },
    ty_def::{BorrowKind, TyBase, TyData, TyId},
    ty_lower::{CompleteSubst, ParamBasis, ParamDomainId, ParamKey, PartialSubst, param_schema},
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::HirAnalysisDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImplSelectionClass<'db> {
    Ordinary,
    Named(IdentId<'db>),
}

/// Selection names classify authored declarations; they are not values or
/// implementation identity. Ordinary discovery uses only unnamed declarations.
pub fn impl_selection_class<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ImplTrait<'db>,
) -> ImplSelectionClass<'db> {
    item.selection_name(db)
        .map_or(ImplSelectionClass::Ordinary, ImplSelectionClass::Named)
}

/// Every named implementation in the ingot with this name, whatever its
/// module or visibility, for tests and diagnostics. A `with` operand is
/// resolved by `path_resolver::resolve_impl_name`, as an item path.
pub fn named_impl_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    name: IdentId<'db>,
) -> Vec<ImplTrait<'db>> {
    ingot_trait_env(db, scope.top_mod(db).ingot(db))
        .named_by_name
        .get(&name)
        .cloned()
        .unwrap_or_default()
}

/// The trait of a named implementation for `ty` that declares an associated
/// type `name`, when path lookup (which reads ordinary implementations) finds
/// none: the trait the qualified form `<ty as Trait>::name` names.
pub(crate) fn named_only_assoc_type_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: TyId<'db>,
    name: IdentId<'db>,
) -> Option<Trait<'db>> {
    ingot_trait_env(db, ingot)
        .named_by_name
        .values()
        .flatten()
        .filter_map(|item| super::trait_lower::lower_impl_trait(db, *item))
        .find(|candidate| {
            candidate.self_ty(db).base_ty(db) == ty.base_ty(db)
                && candidate.trait_def(db).assoc_ty(db, name).is_some()
        })
        .map(|candidate| candidate.trait_def(db))
}

/// The named implementations of `trait_def` in the ingot, kept out of
/// ordinary lookup.
pub(crate) fn named_impls_for_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
) -> Vec<ImplTrait<'db>> {
    let mut out = ingot_trait_env(db, ingot)
        .named_by_name
        .values()
        .flatten()
        .copied()
        .filter(|item| {
            super::trait_lower::lower_impl_trait(db, *item)
                .is_some_and(|candidate| candidate.trait_def(db) == trait_def)
        })
        .collect::<Vec<_>>();
    out.sort_by_key(|item| format!("{item:?}"));
    out
}

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref, cycle_fn=ingot_trait_env_cycle_recover, cycle_initial=ingot_trait_env_cycle_initial)]
pub(crate) fn ingot_trait_env<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all implementors for the given trait definition.
///
/// Note: this intentionally does **not** pre-filter implementors by unifying with a
/// specific goal instance. Projection-heavy goals (e.g. involving associated type
/// projections) often only become unifiable after normalization, and unification
/// rejects unresolved associated types. The solver normalizes before unifying
/// candidates, so any filtering here must be an over-approximation.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
) -> Vec<ImplementorId<'db>> {
    let env = ingot_trait_env(db, ingot);
    let mut out = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) {
        out.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    out.into_iter()
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Different nominal heads cannot unify after freshening or saturation.
/// Keep error types and unknown heads for the full unification check.
fn impl_self_ty_may_match<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_ty: TyId<'db>,
    ty: TyId<'db>,
) -> bool {
    if impl_ty.has_invalid(db) {
        return true;
    }
    let impl_base = impl_ty.base_ty(db);
    let ty_base = ty.base_ty(db);
    match (impl_base.data(db), ty_base.data(db)) {
        (TyData::TyBase(_), TyData::TyBase(_)) => impl_base == ty_base,
        _ => true,
    }
}

/// Returns implementors for `trait_def` whose self type can apply to `ty`.
///
/// Method lookup knows both halves of this key for explicitly selected traits
/// (notably operator traits). Intersecting them before completing candidates is
/// important: receiver-only lookup necessarily includes every blanket and
/// otherwise unindexed impl, whose signatures may themselves require method
/// lookup while being lowered.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_and_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }

    let env = ingot_trait_env(db, ingot);
    let mut implementors = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) && ty.as_contract(db).is_some() {
        implementors.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    implementors
        .into_iter()
        .filter(|implementor| {
            if !impl_self_ty_may_match(db, implementor.self_ty(db), ty) {
                return false;
            }
            let snapshot = table.snapshot();
            let instantiated = table.instantiate_with_fresh_vars(*implementor);
            let impl_ty = table.instantiate_to_term(instantiated.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let unifies = table.unify(impl_ty, ty).is_ok();
            table.rollback_to(snapshot);
            unifies
        })
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Returns all implementors for the given trait inst, searching across a
/// deterministic set of ingots.
///
/// This is used to avoid "pick an ingot" footguns where impl lookup depends on
/// the caller's current module ingot and can miss impls that live in either:
/// - the trait's ingot, or
/// - the implementor type's ingot.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_in_ingots<'db>(
    db: &'db dyn HirAnalysisDb,
    primary: Ingot<'db>,
    secondary: Option<Ingot<'db>>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let trait_def = trait_.extract_identity(&mut table).def(db);
    let mut dedup: IndexSet<ImplementorId<'db>> = IndexSet::default();
    dedup.extend(impls_for_trait_def(db, primary, trait_def).iter().copied());
    if let Some(secondary) = secondary {
        dedup.extend(
            impls_for_trait_def(db, secondary, trait_def)
                .iter()
                .copied(),
        );
    }
    dedup.into_iter().collect()
}

fn is_std_evm_contract_trait_def<'db>(db: &'db dyn HirAnalysisDb, trait_def: Trait<'db>) -> bool {
    let Some(name) = trait_def.name(db).to_opt() else {
        return false;
    };
    if name.data(db) != "Contract" {
        return false;
    }
    if trait_def.top_mod(db).ingot(db).kind(db) != IngotKind::Std {
        return false;
    }
    true
}

#[salsa::tracked(return_ref)]
fn contract_virtual_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Vec<ImplementorId<'db>> {
    let Some(contract_trait) = std_evm_contract_trait_def(db, ingot) else {
        return Vec::new();
    };

    let init_args_ident = IdentId::new(db, "InitArgs".to_string());

    let mut out = Vec::new();
    for top_mod in ingot.all_modules(db) {
        for &contract in top_mod.all_contracts(db).iter() {
            let self_ty = TyId::contract(db, contract);
            let trait_inst = TraitInstId::new_simple(db, contract_trait, vec![self_ty]);

            let init_args_ty = contract.init_args_ty(db);
            let mut types = IndexMap::new();
            types.insert(init_args_ident, init_args_ty);

            let implementor = ImplementorId::new(
                db,
                trait_inst,
                Vec::new(),
                types,
                ImplementorOrigin::VirtualContract(contract),
            );
            out.push(implementor);
        }
    }

    out
}

#[salsa::tracked]
fn std_evm_contract_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Option<Trait<'db>> {
    use crate::analysis::name_resolution::resolve_path;
    use common::ingot::IngotKind;

    let scope = ingot.root_mod(db).scope();
    let assumptions = PredicateListId::empty_list(db);

    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let path = crate::hir_def::PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "evm".to_string()))
        .push_ident(db, IdentId::new(db, "Contract".to_string()));

    match resolve_path(db, path, scope, assumptions, false).ok()? {
        crate::analysis::name_resolution::PathRes::Trait(inst) => Some(inst.def(db)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ImplementorOrigin<'db> {
    Hir(ImplTrait<'db>),
    VirtualContract(Contract<'db>),
    Assumption,
    /// A callable value's implementation of its callable shape traits: a
    /// closure's or a function item's (`ty::closure`).
    Callable,
    /// The compiler's structural implementation of a marker trait
    /// (`ty::clearable`).
    Structural,
}

fn ingot_trait_env_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitEnv<'db> {
    // Return an empty trait environment when we detect a cycle
    TraitEnv {
        impls: FxHashMap::default(),
        impls_by_self: FxHashMap::default(),
        unindexed_self_impls: Vec::new(),
        named_by_name: FxHashMap::default(),
        ingot,
    }
}

fn ingot_trait_env_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TraitEnv<'db>,
    _count: u32,
    _ingot: Ingot<'db>,
) -> salsa::CycleRecoveryAction<TraitEnv<'db>> {
    // Continue iterating to try to resolve the cycle
    salsa::CycleRecoveryAction::Iterate
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ResolvedImplInstance<'db> {
    selected: ImplementorId<'db>,
    instantiated: ImplementorId<'db>,
    trait_inst: TraitInstId<'db>,
}

impl<'db> ResolvedImplInstance<'db> {
    pub fn selected(self) -> ImplementorId<'db> {
        self.selected
    }

    pub fn trait_inst(self) -> TraitInstId<'db> {
        self.trait_inst
    }

    pub fn impl_args(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.instantiated.params(db)
    }

    pub fn impl_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.selected.params(db)
    }

    /// Resolves an implemented trait method, called at `mode`, using this
    /// already-selected impl.
    ///
    /// The returned generic arguments match the selected implementation for an
    /// explicit method and the resolved trait instance for a default method.
    fn method_instance(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        mode: Option<BorrowKind>,
    ) -> Option<(Func<'db>, Vec<TyId<'db>>)> {
        let implementor = self.selected();
        if let Some(func) = implementor.method_at(db, name, mode) {
            return Some((func, self.impl_args(db).to_vec()));
        }
        let func = implementor
            .trait_def(db)
            .method_defs(db)
            .get(&name)
            .copied()
            .filter(|method| method.body(db).is_some())?;
        Some((func, self.trait_inst().args(db).to_vec()))
    }

    pub fn assoc_ty_template(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.selected.assoc_ty(db, name)
    }

    pub fn instantiated_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.instantiated.assoc_ty(db, name)
    }
}

impl<'db> TyFoldable<'db> for ResolvedImplInstance<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            selected: self.selected,
            instantiated: self.instantiated.fold_with(db, folder),
            trait_inst: self.trait_inst.fold_with(db, folder),
        }
    }
}

impl<'db> TyVisitable<'db> for ResolvedImplInstance<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.instantiated.visit_with(visitor);
        self.trait_inst.visit_with(visitor);
    }
}

pub(crate) fn instantiate_selected_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    if matches!(selected.origin(db), ImplementorOrigin::Assumption) {
        return Some(ResolvedImplInstance {
            selected,
            instantiated: selected,
            trait_inst: inst,
        });
    }

    // The selection proved the goal's associated-type bindings, which only
    // normalization relates to the implementation's; its arguments follow
    // from the trait's.
    let mut table = UnificationTable::new(db);
    let instantiated = table.instantiate_with_fresh_vars(selected);
    for (&impl_arg, &arg) in instantiated
        .trait_inst(db)
        .args(db)
        .iter()
        .zip(inst.args(db))
    {
        table.unify(impl_arg, arg).ok()?;
    }
    Some(ResolvedImplInstance {
        selected,
        instantiated: instantiated.fold_with(db, &mut table),
        trait_inst: inst.fold_with(db, &mut table),
    })
}

/// Ordinary selection: the implementation that answers `inst` when no choice
/// is read. Every caller names why that is right where it calls
/// (`OrdinarySelection`).
pub(super) fn resolve_trait_impl_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    _why: super::trait_choice::OrdinarySelection,
) -> Selection<ResolvedImplInstance<'db>> {
    match resolve_trait_impl_instance_query(db, solve_cx, inst) {
        ImplSelection::Unique(resolved) => Selection::Unique(resolved),
        ImplSelection::Ambiguous => Selection::Ambiguous(IndexSet::new()),
        ImplSelection::NotFound => Selection::NotFound,
    }
}

/// The implementation that gives `inst`'s slot its associated types,
/// effect rows and result spaces: the ordinary one, or, for a slot with only
/// named implementations, any named one whose header matches
/// (`named_output_implementation`). Readers of associated outputs select
/// through this, not through ordinary selection alone.
pub(crate) fn output_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Selection<ResolvedImplInstance<'db>> {
    match resolve_trait_impl_instance(
        db,
        solve_cx,
        inst,
        super::trait_choice::OrdinarySelection::slot_outputs(),
    ) {
        Selection::NotFound => named_output_implementation(db, solve_cx, inst),
        ordinary => ordinary,
    }
}

/// The outputs of a slot that ordinary selection does not answer, from its
/// named implementations. A named declaration is admitted only when its
/// outputs equal the ordinary implementation's or, without one, every other
/// named implementation's, so whichever named implementation answers the slot
/// gives these outputs; its conditions do not matter for them.
pub(crate) fn named_output_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Selection<ResolvedImplInstance<'db>> {
    if inst.args(db).iter().any(|ty| ty.has_var(db)) {
        return Selection::NotFound;
    }
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst =
        normalize_trait_inst_preserving_validity(db, inst, norm_scope, solve_cx.assumptions());
    let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(db, inst);
    [Some(primary), secondary]
        .into_iter()
        .flatten()
        .flat_map(|ingot| named_impls_for_trait(db, ingot, inst.def(db)))
        .filter_map(|item| super::trait_lower::lower_impl_trait(db, item))
        .find_map(|candidate| {
            complete_selected_impl(db, candidate)
                .and_then(|completed| instantiate_selected_impl(db, completed, inst))
        })
        .map_or(Selection::NotFound, Selection::Unique)
}

/// An implementation selection, without candidates for an ambiguous one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
enum ImplSelection<'db> {
    Unique(ResolvedImplInstance<'db>),
    Ambiguous,
    NotFound,
}

/// Call sites and method checks select the same implementations repeatedly.
#[salsa::tracked]
fn resolve_trait_impl_instance_query<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> ImplSelection<'db> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);
    match solve_cx.select_impl(db, inst) {
        Selection::Unique(selected) => complete_selected_impl(db, selected)
            .and_then(|selected| instantiate_selected_impl(db, selected, inst))
            .map_or(ImplSelection::NotFound, ImplSelection::Unique),
        // There is deliberately no resolved instance for a non-unique
        // selection. Re-unifying each candidate would both fabricate evidence
        // the caller must not consume and attempt to fold inference variables
        // owned by the caller through a fresh local table.
        Selection::Ambiguous(_) => ImplSelection::Ambiguous,
        Selection::NotFound => ImplSelection::NotFound,
    }
}

/// A borrowed, checked application of one implementation. This is compiler
/// state, not a language Evidence value or a cross-revision certificate.
/// Its only constructor proves that candidate through the ordinary solver.
pub struct CheckedImplApplication<'db> {
    db: &'db dyn HirAnalysisDb,
    origin: TraitSolveCx<'db>,
    candidate: ImplementorId<'db>,
    goal: TraitInstId<'db>,
    resolved: ResolvedImplInstance<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckedApplicationError {
    UnsupportedInput,
    UnsupportedConditions,
    ConditionsNotAdmitted,
    NotProven,
    Incomplete,
    UnresolvedApplication,
}

impl std::fmt::Debug for CheckedImplApplication<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CheckedImplApplication")
            .field("candidate", &self.candidate)
            .field("goal", &self.goal)
            .field("origin", &self.origin)
            .finish()
    }
}

fn checked_type<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    !ty.has_invalid(db) && !ty.has_var(db) && !ty.has_projection(db)
}

/// Header reconstruction alone is deliberately not a checked application.
/// Proof conditions are checked outside solving through master's ordinary
/// declaration admission. Unsupported generic conditions remain Unsupported.
pub(crate) fn prove_selected_application<'db>(
    db: &'db dyn HirAnalysisDb,
    origin: TraitSolveCx<'db>,
    candidate: ImplementorId<'db>,
    goal: TraitInstId<'db>,
) -> Result<CheckedImplApplication<'db>, CheckedApplicationError> {
    use CheckedApplicationError::*;
    if matches!(candidate.origin(db), ImplementorOrigin::Assumption)
        || goal
            .args(db)
            .iter()
            .chain(goal.assoc_type_bindings(db).values())
            .any(|ty| !checked_type(db, *ty))
    {
        return Err(UnsupportedInput);
    }
    if super::trait_resolution::goal_has_const_type_requirements(db, goal) {
        return Err(UnsupportedConditions);
    }
    let conditions = match origin.restricted_goal(db, goal, candidate) {
        GoalSatisfiability::Satisfied(solution) => solution.value.impl_conditions,
        GoalSatisfiability::NeedsConfirmation { .. } => return Err(Incomplete),
        GoalSatisfiability::ContainsInvalid => return Err(UnsupportedInput),
        GoalSatisfiability::UnSat(_) => return Err(NotProven),
    };
    let completed = complete_selected_impl(db, candidate).ok_or(UnsupportedInput)?;
    let scope = origin.normalization_scope_for_trait_inst(db, goal);
    let normalized =
        normalize_trait_inst_preserving_validity(db, goal, scope, origin.assumptions());
    let resolved =
        instantiate_selected_impl(db, completed, normalized).ok_or(UnresolvedApplication)?;
    // An associated type written as a projection is checked as what it
    // normalizes to (`type Out = <Item as Has>::Out` is `u8`).
    let assoc_types = resolved
        .instantiated
        .types(db)
        .values()
        .map(|ty| normalize_ty(db, *ty, scope, origin.assumptions()))
        .collect::<Vec<_>>();
    if resolved
        .impl_args(db)
        .iter()
        .chain(assoc_types.iter())
        .chain(resolved.trait_inst().args(db))
        .chain(resolved.trait_inst().assoc_type_bindings(db).values())
        .any(|ty| !checked_type(db, *ty))
    {
        return Err(UnresolvedApplication);
    }
    // Use the complete existing receipt, including prerequisite impls,
    // after solving. Master's checker retains source admission policy:
    // ground true is admitted; generic/false/blocked/failed is not.
    for instance in conditions.instances(db) {
        if super::trait_resolution::goal_has_const_type_requirements(db, instance.trait_inst(db)) {
            return Err(UnsupportedConditions);
        }
        let ImplementorOrigin::Hir(implementation) = instance.origin(db) else {
            return Err(ConditionsNotAdmitted);
        };
        if !super::ty_check::check_where_const_predicates(
            db,
            crate::hir_def::WhereClauseOwner::ImplTrait(implementation),
        )
        .is_empty()
        {
            return Err(ConditionsNotAdmitted);
        }
    }
    for goal in conditions.trait_goals(db) {
        if super::trait_resolution::goal_has_const_type_requirements(db, *goal) {
            return Err(UnsupportedConditions);
        }
        if !super::ty_check::check_where_const_predicates(
            db,
            crate::hir_def::WhereClauseOwner::Trait(goal.def(db)),
        )
        .is_empty()
        {
            return Err(ConditionsNotAdmitted);
        }
    }
    Ok(CheckedImplApplication {
        db,
        origin,
        candidate,
        goal,
        resolved,
    })
}

/// Check one authored implementation with the ordinary proof and admission
/// machinery. This is a compiler API, not source Fe evidence or an execution
/// certificate. Declarations and IDs must belong to `db`; callers still perform
/// ordinary source/coherence, method requirement and body checks.
///
/// Generic impl const conditions and conditional nominal type applications are
/// not admitted by this preparatory interface. Rigid type bounds are supported.
pub fn prove_selected_hir_application<'db>(
    db: &'db dyn HirAnalysisDb,
    origin: TraitSolveCx<'db>,
    implementation: ImplTrait<'db>,
    goal: TraitInstId<'db>,
) -> Result<CheckedImplApplication<'db>, CheckedApplicationError> {
    let candidate = super::trait_lower::lower_impl_trait(db, implementation)
        .ok_or(CheckedApplicationError::UnsupportedInput)?;
    prove_selected_application(db, origin, candidate, goal)
}

impl<'db> CheckedImplApplication<'db> {
    /// The complete requested goal, including associated equalities.
    pub fn goal(&self) -> TraitInstId<'db> {
        self.goal
    }

    pub fn resolved(&self) -> ResolvedImplInstance<'db> {
        self.resolved
    }
    pub fn origin_context(&self) -> TraitSolveCx<'db> {
        self.origin
    }
    pub fn associated_type(&self, name: IdentId<'db>) -> Option<TyId<'db>> {
        self.resolved.instantiated_assoc_ty(self.db, name)
    }
    pub fn method(&self, name: IdentId<'db>) -> Selection<ResolvedMethodInstance<'db>> {
        method_instance_from_resolved(self.db, self.resolved, name, None)
    }
    pub fn revalidate(
        &self,
        destination: TraitSolveCx<'db>,
        goal: TraitInstId<'db>,
    ) -> Result<Self, CheckedApplicationError> {
        // Do not copy source assumptions into the destination or switch to a
        // different implementation when this candidate loses a prerequisite.
        prove_selected_application(self.db, self.origin, self.candidate, self.goal)?;
        prove_selected_application(self.db, destination, self.candidate, goal)
    }
}

/// What a named implementation's declaration gives differently from another
/// implementation of its slot.
pub(crate) enum NamedOutputMismatch<'db> {
    /// An associated type; `with` is the implementation compared with.
    AssocType {
        with: ImplTrait<'db>,
        ordinary: bool,
        name: IdentId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },
    /// An associated effect row.
    Row {
        with: ImplTrait<'db>,
        ordinary: bool,
        name: IdentId<'db>,
    },
}

/// The traits whose implementation is chosen where no frame reaches it, in
/// generated code: storage locations and their lanes (`StateIndex`,
/// `LaneCodec`, the storage map's `StorageKey`), encodings (`Abi`, `AbiSize`,
/// `Encode`, `Decode`, `ErrorVariant`) and the contract host
/// (`ContractHost`). A named implementation of one would put a value at
/// another location, or encode it another way, under a `with` than without
/// it, so they have none. Every core trait lowering resolves is listed here
/// (a guard checks it).
const UNIQUE_CORE_TRAITS: [&[&str]; 8] = [
    &["ops", "StateIndex"],
    &["ops", "LaneCodec"],
    &["abi", "Abi"],
    &["abi", "AbiSize"],
    &["abi", "Encode"],
    &["abi", "Decode"],
    &["error", "ErrorVariant"],
    &["contracts", "ContractHost"],
];

/// Whether `trait_` may have no named implementation: one of
/// `UNIQUE_CORE_TRAITS`, or the storage map's `StorageKey`.
pub(crate) fn has_only_ordinary_implementations<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    trait_: crate::hir_def::Trait<'db>,
) -> bool {
    UNIQUE_CORE_TRAITS
        .iter()
        .any(|path| super::corelib::resolve_core_trait(db, scope, path) == Some(trait_))
        || super::corelib::resolve_lib_trait_path(db, scope, "std::evm::storage_map::StorageKey")
            == Some(trait_)
}

/// A slot's associated types and effect rows do not depend on which implementation answers it. A named
/// declaration is compared with every other implementation of the trait,
/// ordinary or named, whose header overlaps its own, at their overlap and
/// whatever their conditions: a condition that does not hold here can hold
/// where a `with` adds a premise, and two named implementations that both
/// answer a slot would otherwise give it outputs that depend on which is
/// found first. This is definition admission, independent of any opening.
pub(crate) fn named_output_mismatch<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
    candidate: ImplementorId<'db>,
) -> Option<NamedOutputMismatch<'db>> {
    let context = TraitSolveCx::new(db, implementation.scope())
        .with_assumptions(crate::semantic::param_env(db, implementation.into()));
    let header = candidate.trait_inst(db);
    let goal = TraitInstId::new_simple(db, header.def(db), header.args(db).clone());
    let (primary, secondary) = context.search_ingots_for_trait_inst(db, goal);
    let ingots = [Some(primary), secondary]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    let ordinary = ingots
        .iter()
        .flat_map(|ingot| impls_for_trait_def(db, *ingot, goal.def(db)).to_vec())
        .filter(|other| matches!(other.origin(db), ImplementorOrigin::Hir(_)))
        .map(|other| (other, true));
    let named = ingots
        .iter()
        .flat_map(|ingot| named_impls_for_trait(db, *ingot, goal.def(db)))
        .filter(|other| *other != implementation)
        .filter_map(|other| super::trait_lower::lower_impl_trait(db, other))
        .map(|other| (other, false));
    let others = ordinary.chain(named).collect::<Vec<_>>();
    for (other, ordinary) in others {
        let ImplementorOrigin::Hir(with) = other.origin(db) else {
            continue;
        };
        let Some((own, other)) = at_overlap(db, candidate, other) else {
            continue;
        };
        for assoc in goal.def(db).assoc_types(db) {
            let Some(name) = assoc.name(db) else {
                continue;
            };
            let (Some(expected), Some(given)) = (
                other.instantiated_assoc_ty(db, name),
                own.instantiated_assoc_ty(db, name),
            ) else {
                continue;
            };
            let expected = consts_evaluated(
                db,
                normalize_ty(db, expected, implementation.scope(), context.assumptions()),
            );
            let given = consts_evaluated(
                db,
                normalize_ty(db, given, implementation.scope(), context.assumptions()),
            );
            if expected.has_invalid(db) || given.has_invalid(db) {
                continue;
            }
            // Equal at the overlap, for every value of what the overlap
            // leaves open: the two are compared as they are, not unified.
            if expected != given {
                return Some(NamedOutputMismatch::AssocType {
                    with,
                    ordinary,
                    name,
                    expected,
                    given,
                });
            }
        }
        for row in goal.def(db).rows(db) {
            let Some(name) = row.name.to_opt() else {
                continue;
            };
            let components = |resolved| {
                super::effects::rows::implementation_row_components(db, resolved, name).map(
                    |components| {
                        components
                            .into_iter()
                            .map(|component| (component.name, component.key, component.is_mut))
                            .collect::<Vec<_>>()
                    },
                )
            };
            if components(other) != components(own) {
                return Some(NamedOutputMismatch::Row {
                    with,
                    ordinary,
                    name,
                });
            }
        }
    }
    None
}

/// `ty` with each constant in it evaluated, so two written forms of one type
/// (`Arr<{ 100 }>` and `Arr<100>`, or one braced constant written twice)
/// compare equal.
fn consts_evaluated<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    struct Evaluate;
    impl<'db> TyFolder<'db> for Evaluate {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let ty = ty.super_fold_with(db, self);
            super::const_ty::normalize_const_tys_for_comparison(db, ty)
        }
    }
    ty.fold_with(db, &mut Evaluate)
}

/// Two implementations instantiated at their overlap: each header with fresh
/// variables, unified with the other. `None` when the headers cannot both
/// match one goal.
fn at_overlap<'db>(
    db: &'db dyn HirAnalysisDb,
    own: ImplementorId<'db>,
    other: ImplementorId<'db>,
) -> Option<(ResolvedImplInstance<'db>, ResolvedImplInstance<'db>)> {
    let mut table = UnificationTable::new(db);
    let own_fresh = table.instantiate_with_fresh_vars(own);
    let other_fresh = table.instantiate_with_fresh_vars(other);
    for (&a, &b) in own_fresh
        .trait_inst(db)
        .args(db)
        .iter()
        .zip(other_fresh.trait_inst(db).args(db))
    {
        table.unify(a, b).ok()?;
    }
    let resolved = |selected, fresh: ImplementorId<'db>, table: &mut UnificationTable<'db>| {
        let instantiated = fresh.fold_with(db, table);
        ResolvedImplInstance {
            selected,
            instantiated,
            trait_inst: instantiated.trait_inst(db),
        }
    };
    let own = resolved(own, own_fresh, &mut table);
    let other = resolved(other, other_fresh, &mut table);
    Some((own, other))
}

/// A selected method retains the proof origin and the nominal trait reference.
/// A trait default body selected through an assumption remains generic-bound
/// dispatch until a concrete implementation is chosen.
#[derive(Debug, Clone)]
pub struct ResolvedMethodInstance<'db> {
    resolved: ResolvedImplInstance<'db>,
    declaration: Func<'db>,
    body: Option<Func<'db>>,
    body_args: Vec<TyId<'db>>,
    body_to_nominal: Vec<Option<usize>>,
    /// The declaration's effects paired with the body's, absent when the body
    /// is the declaration.
    effect_pairs: Option<Vec<(usize, usize)>>,
}

#[derive(Debug, Clone)]
pub enum MethodArgMapError<'db> {
    MissingBody,
    /// An evidence parameter of the body that runs, which its call did not
    /// deliver.
    UndeliveredEvidence {
        func: Func<'db>,
        param: usize,
    },
    NominalArity {
        expected: usize,
        given: usize,
    },
    SelectedPrefixArity {
        expected: usize,
        given: usize,
    },
    MissingNominalRole(ParamKey<'db>),
    MissingEffectRole(usize),
    MissingProviderRole(u32),
    InconsistentProviderRole {
        nominal: u32,
        first: u32,
        second: u32,
    },
    SignatureArity {
        func: Func<'db>,
        expected_generics: usize,
        given_generics: usize,
        declared_inputs: usize,
        parameter_inputs: usize,
    },
    SignatureMismatch {
        nominal: Func<'db>,
        body: Func<'db>,
        nominal_args: Vec<TyId<'db>>,
        body_args: Vec<TyId<'db>>,
        evidence: PredicateListId<'db>,
        nominal_signature: String,
        body_signature: String,
    },
}

impl<'db> ResolvedMethodInstance<'db> {
    pub fn resolved(&self) -> ResolvedImplInstance<'db> {
        self.resolved
    }

    pub fn body(&self) -> Option<Func<'db>> {
        self.body
    }

    pub fn body_args(&self) -> &[TyId<'db>] {
        &self.body_args
    }

    pub fn effect_pairs(&self) -> Option<&[(usize, usize)]> {
        self.effect_pairs.as_deref()
    }

    pub fn origin(&self, db: &'db dyn HirAnalysisDb) -> ImplementorOrigin<'db> {
        self.resolved.selected().origin(db)
    }

    /// Rebase an already-completed nominal method instance into the selected
    /// body schema. Inherited values come from implementation selection; the
    /// method's own and hidden slots are matched by structural role.
    pub fn complete_body_args(
        &self,
        db: &'db dyn HirAnalysisDb,
        nominal_args: &[TyId<'db>],
    ) -> Result<CompleteSubst<'db>, MethodArgMapError<'db>> {
        let body = self.body.ok_or(MethodArgMapError::MissingBody)?;
        let nominal_schema = param_schema(db, self.declaration.into(), ParamBasis::Full);
        if nominal_args.len() != nominal_schema.keys(db).len() {
            return Err(MethodArgMapError::NominalArity {
                expected: nominal_schema.keys(db).len(),
                given: nominal_args.len(),
            });
        }
        let body_schema = param_schema(db, body.into(), ParamBasis::Full);
        let inherited_len = GenericParamOwner::Func(body)
            .parent(db)
            .map_or(0, |parent| {
                param_schema(db, parent, ParamBasis::Full).keys(db).len()
            });
        if self.body_args.len() != inherited_len {
            return Err(MethodArgMapError::SelectedPrefixArity {
                expected: inherited_len,
                given: self.body_args.len(),
            });
        }

        let domain = ParamDomainId::full(db, body_schema);
        let mut args = PartialSubst::new(db, domain);
        for (slot, &arg) in self.body_args.iter().enumerate() {
            args.bind(db, body_schema.keys(db)[slot], arg)
                .expect("selected inherited parameter key");
        }
        for (body_slot, &key) in body_schema.keys(db).iter().enumerate().skip(inherited_len) {
            if let Some(nominal_slot) = self.body_to_nominal.get(body_slot).copied().flatten() {
                args.bind(db, key, nominal_args[nominal_slot])
                    .expect("selected nominal parameter key");
            } else if !matches!(key, ParamKey::EffectProvider { func, effect_idx }
                    if func.effect_from_row(db, effect_idx))
            {
                return Err(MethodArgMapError::MissingNominalRole(key));
            }
        }
        // The call provides the effects of an implementation's rows, so their
        // provider slots keep their body formals.
        Ok(args.residualize(db))
    }
}

/// Selects a trait method while retaining implementation versus generic-bound
/// origin. `body` is absent when only a declaration is available.
pub fn resolve_trait_method_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
    mode: Option<BorrowKind>,
    why: super::trait_choice::OrdinarySelection,
) -> Selection<ResolvedMethodInstance<'db>> {
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst, why) {
        Selection::Unique(resolved) => resolved,
        Selection::Ambiguous(_) => return Selection::Ambiguous(IndexSet::new()),
        Selection::NotFound => return Selection::NotFound,
    };
    method_instance_from_resolved(db, resolved, method, mode)
}

/// The method body that runs for a trait call whose decision, read in the
/// instance that makes the call, is `chosen`: the known choice's
/// implementation applied to `inst` (normalized as the instance key
/// normalizes it), ordinary selection when there is no decision, and nothing
/// for a pending choice. With `chosen_constant` and `candidate_methods`, the
/// one way from a choice to a body: the template functions they read are
/// private here, so a body chosen by a choice is reached only through them.
/// The choices the body receives are mapped by `trait_choice::body_deliveries`
/// from the same call.
pub(crate) fn chosen_method<'db>(
    db: &'db dyn HirAnalysisDb,
    chosen: super::trait_choice::Resolved<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    mode: Option<BorrowKind>,
) -> Selection<ResolvedMethodInstance<'db>> {
    use super::trait_choice::{OrdinarySelection, Resolved};
    match chosen {
        Resolved::Known(known) => {
            let inst = normalize_trait_inst_preserving_validity(db, inst, scope, assumptions);
            known_instance(db, known, inst).map_or(Selection::NotFound, |resolved| {
                method_instance_from_resolved(db, resolved, method, mode)
            })
        }
        Resolved::Ordinary => resolve_trait_method_instance(
            db,
            TraitSolveCx::new(db, scope).with_assumptions(assumptions),
            inst,
            method,
            mode,
            OrdinarySelection::no_recorded_choice(),
        ),
        Resolved::Pending => Selection::NotFound,
    }
}

/// The implementation a constant use reads, and its constant body with the
/// choices that body receives: the use's recorded choice (known), ordinary
/// selection when there is no decision or the decision is the reading body's
/// own bound, and nothing for a pending choice, which is never answered the
/// ordinary way. The body is the selected implementation's constant or
/// the trait's default, absent for a built-in constant; its choices are the
/// use's deliveries mapped to it (`DeliveryList::to_body`).
pub(crate) fn chosen_constant<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: super::assoc_const::AssocConstUse<'db>,
) -> Option<ChosenConstant<'db>> {
    use super::trait_choice::{Choice, SlotChoices, SlotRef};
    let resolved = match use_.choice() {
        Some(Choice::Known(known)) => known_instance(
            db,
            known,
            normalize_trait_inst_preserving_validity(
                db,
                use_.inst(),
                use_.origin_scope(),
                use_.assumptions(),
            ),
        )?,
        Some(Choice::Slot(SlotRef::Bound(_))) | None => match resolve_trait_impl_instance(
            db,
            use_.solve_cx(db),
            use_.inst(),
            super::trait_choice::OrdinarySelection::no_recorded_choice(),
        ) {
            Selection::Unique(resolved) => resolved,
            Selection::Ambiguous(_) | Selection::NotFound => return None,
        },
        Some(Choice::Slot(_)) => return None,
    };
    let body =
        selected_assoc_const_body_template(db, resolved, use_.name()).map(|(body, ty, subst)| {
            ConstantBody {
                body,
                ty,
                subst,
                choices: use_
                    .delivered()
                    .map_or_else(SlotChoices::default, |list| list.to_body(db, body)),
            }
        });
    Some(ChosenConstant { resolved, body })
}

/// The ordinary implementation of `inst` and its constant `name`'s body, for
/// a read that no choice can reach, named by `why`. Its body receives
/// nothing.
pub(crate) fn ordinary_constant<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
    why: super::trait_choice::OrdinarySelection,
) -> Option<ChosenConstant<'db>> {
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, inst, why) else {
        return None;
    };
    let body = selected_assoc_const_body_template(db, resolved, name).map(|(body, ty, subst)| {
        ConstantBody {
            body,
            ty,
            subst,
            choices: super::trait_choice::SlotChoices::default(),
        }
    });
    Some(ChosenConstant { resolved, body })
}

/// The implementation a constant read selects, and the body it runs
/// (`chosen_constant`, `ordinary_constant`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ChosenConstant<'db> {
    /// The implementation applied to the read's goal.
    pub resolved: ResolvedImplInstance<'db>,
    /// Absent for a constant the compiler provides.
    pub body: Option<ConstantBody<'db>>,
}

/// A constant body with its declared type, the arguments of its owner, and
/// what its bounds receive from the read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConstantBody<'db> {
    pub body: Body<'db>,
    pub ty: TyId<'db>,
    pub subst: CompleteSubst<'db>,
    pub choices: super::trait_choice::SlotChoices<'db>,
}

/// The method bodies a trait call whose implementation is chosen later could
/// run: `method` of every implementation that applies to `inst` as written
/// (`applicable_impls`), read for the checks such a call owes. No choice is
/// read, so none is received.
pub(crate) fn candidate_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
    mode: Option<BorrowKind>,
) -> Vec<ResolvedMethodInstance<'db>> {
    applicable_impls(db, ingot, inst)
        .into_iter()
        .filter_map(
            |resolved| match method_instance_from_resolved(db, resolved, method, mode) {
                Selection::Unique(method) => Some(method),
                _ => None,
            },
        )
        .collect()
}

/// The constant bodies a constant read whose implementation is chosen later
/// could run, as `candidate_methods` for a call.
pub(crate) fn candidate_constants<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Vec<(Body<'db>, TyId<'db>, CompleteSubst<'db>)> {
    applicable_impls(db, ingot, inst)
        .into_iter()
        .filter_map(|resolved| selected_assoc_const_body_template(db, resolved, name))
        .collect()
}

/// The implementations, ordinary and named, that apply to `inst` as written,
/// each applied to it.
fn applicable_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    inst: TraitInstId<'db>,
) -> Vec<ResolvedImplInstance<'db>> {
    let (primary, secondary) =
        TraitSolveCx::search_ingots_for_trait_inst_with_origin(db, ingot, inst);
    let mut search_ingots = Vec::new();
    for candidate_ingot in [Some(ingot), Some(primary), secondary]
        .into_iter()
        .flatten()
    {
        if !search_ingots.contains(&candidate_ingot) {
            search_ingots.push(candidate_ingot);
        }
    }
    let mut candidates: IndexSet<ImplementorId<'db>> = IndexSet::default();
    for search_ingot in search_ingots {
        candidates.extend(
            impls_for_trait_def(db, search_ingot, inst.def(db))
                .iter()
                .copied(),
        );
        candidates.extend(
            named_impls_for_trait(db, search_ingot, inst.def(db))
                .into_iter()
                .filter_map(|item| super::trait_lower::lower_impl_trait(db, item)),
        );
    }
    candidates
        .into_iter()
        .filter(|candidate| matches!(candidate.origin(db), ImplementorOrigin::Hir(_)))
        .filter(|candidate| may_apply(db, *candidate, inst))
        .filter_map(|candidate| {
            complete_selected_impl(db, candidate)
                .and_then(|completed| instantiate_selected_impl(db, completed, inst))
        })
        .collect()
}

/// Whether `candidate`, as lowered for lookup, may apply to `inst`. Its
/// trait arguments are the checked lowering's unless they hold a constant or
/// a projection, which only the checked lowering settles; a candidate whose
/// arguments hold neither and do not unify with `inst` does not apply, and
/// is not lowered again (`complete_selected_impl` is not cached).
fn may_apply<'db>(
    db: &'db dyn HirAnalysisDb,
    candidate: ImplementorId<'db>,
    inst: TraitInstId<'db>,
) -> bool {
    struct Unsettled<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Unsettled<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_const_ty(&mut self, const_ty: &super::const_ty::ConstTyId<'db>) {
            if !matches!(
                const_ty.data(self.db),
                super::const_ty::ConstTyData::TyParam(..)
            ) {
                self.found = true;
            }
            super::visitor::walk_const_ty(self, const_ty)
        }
    }
    let mut unsettled = Unsettled { db, found: false };
    for ty in candidate.trait_inst(db).args(db) {
        if ty.has_projection(db) {
            return true;
        }
        unsettled.visit_ty(*ty);
    }
    unsettled.found || instantiate_selected_impl(db, candidate, inst).is_some()
}

/// A known choice's implementation applied to `inst`, a goal in its slot.
fn known_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    known: super::trait_choice::KnownChoice<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    let candidate = complete_selected_impl(
        db,
        super::trait_lower::lower_impl_trait(db, known.implementation())?,
    )?;
    instantiate_selected_impl(db, candidate, inst)
}

/// Assemble method metadata from one reconstructed application. Retained
/// consumers use this path without running implementation discovery again.
fn method_instance_from_resolved<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: ResolvedImplInstance<'db>,
    method: IdentId<'db>,
    mode: Option<BorrowKind>,
) -> Selection<ResolvedMethodInstance<'db>> {
    let Some(declaration) = resolved
        .selected()
        .trait_def(db)
        .method_defs(db)
        .get(&method)
        .copied()
    else {
        return Selection::NotFound;
    };
    let (body, body_args) = resolved.method_instance(db, method, mode).map_or_else(
        || (None, resolved.trait_inst().args(db).to_vec()),
        |(func, args)| (Some(func), args),
    );
    let (body_to_nominal, effect_pairs) = body.map_or_else(
        || (Vec::new(), Some(Vec::new())),
        |body| {
            if body == declaration {
                (
                    (0..param_schema(db, body.into(), ParamBasis::Full)
                        .keys(db)
                        .len())
                        .map(Some)
                        .collect(),
                    None,
                )
            } else {
                let correspondence = method_param_correspondence(
                    db,
                    body.as_callable(db).expect("selected method is callable"),
                    declaration
                        .as_callable(db)
                        .expect("trait method is callable"),
                    resolved.selected().trait_inst(db),
                );
                (
                    correspondence.body_to_trait_slots,
                    Some(correspondence.trait_effect_to_body),
                )
            }
        },
    );
    Selection::Unique(ResolvedMethodInstance {
        resolved,
        declaration,
        body,
        body_args,
        body_to_nominal,
        effect_pairs,
    })
}

/// Returns all implementors for the given `ty` whose constraints are fully proven.
pub(crate) fn impls_for_ty_with_satisfied_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);
    impls_with_constraint_mode(db, ingot, &mut table, ty, None, assumptions, false)
}

/// Returns implementors of `inst`'s trait that can apply to `inst` and whose
/// constraints, under `inst`'s arguments, are not known to be unsatisfied.
pub(crate) fn impls_for_trait_inst_with_possible_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    inst: Canonical<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let inst = inst.extract_identity(&mut table);
    let ty = inst.self_ty(db);
    impls_with_constraint_mode(db, ingot, &mut table, ty, Some(inst), assumptions, true)
}

fn impls_with_constraint_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    table: &mut UnificationTable<'db>,
    ty: TyId<'db>,
    trait_inst: Option<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
    allow_needs_confirmation: bool,
) -> Vec<ImplementorId<'db>> {
    let solve_cx = TraitSolveCx::new(db, ingot.root_mod(db).scope()).with_assumptions(assumptions);
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let trait_def = trait_inst.map(|inst| inst.def(db));
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = match trait_def {
        Some(trait_def) => env.impls_for_trait(db, trait_def),
        None => env.impls_for_self_key(db, ty.base_ty(db)),
    };

    if ty.as_contract(db).is_some()
        && trait_def.is_none_or(|trait_def| is_std_evm_contract_trait_def(db, trait_def))
    {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            if !impl_self_ty_may_match(db, impl_.self_ty(db), ty) {
                return false;
            }
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            // The constraints are checked under the target's arguments too:
            // `impl<B, D: Driver<B>> Driver<B> for Filter<D>` needs its `B`.
            let unifies =
                table.unify(impl_ty, ty_term).is_ok()
                    && trait_inst.is_none_or(|target| {
                        inst.trait_(db).args(db).iter().zip(target.args(db)).all(
                            |(&impl_arg, &arg)| {
                                let arg = table.instantiate_to_term(arg);
                                table.unify(impl_arg, arg).is_ok()
                            },
                        )
                    });

            if unifies {
                let impl_constraints = inst.constraints(db);
                if impl_constraints.is_empty(db) {
                    table.rollback_to(snapshot);
                    return true;
                }

                for &constraint in impl_constraints.list(db) {
                    let constraint = constraint.fold_with(db, table);
                    let satisfiability = is_goal_satisfiable(db, solve_cx, constraint);
                    let constraint_holds =
                        matches!(satisfiability, GoalSatisfiability::Satisfied(_))
                            || (allow_needs_confirmation
                                && matches!(
                                    satisfiability,
                                    GoalSatisfiability::NeedsConfirmation { .. }
                                ));
                    if !constraint_holds {
                        table.rollback_to(snapshot);
                        return false;
                    }
                }
            }

            table.rollback_to(snapshot);
            unifies
        })
        .collect()
}

/// Returns all implementors for the given `ty`.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = env.impls_for_self_key(db, ty.base_ty(db));

    if ty.as_contract(db).is_some() {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty_term).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Returns the selected body, its declared template type, and the checked
/// substitution over the body's owner. Explicit impl bodies use impl
/// arguments; inherited defaults use trait arguments.
pub(super) fn assoc_const_body_template_for_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    const_name: IdentId<'db>,
) -> Option<(Body<'db>, TyId<'db>, CompleteSubst<'db>)> {
    let resolved = match resolve_trait_impl_instance(
        db,
        solve_cx,
        inst,
        super::trait_choice::OrdinarySelection::outermost_tier_only(),
    ) {
        Selection::Unique(resolved) => resolved,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound => return None,
    };
    selected_assoc_const_body_template(db, resolved, const_name)
}

fn selected_assoc_const_body_template<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: ResolvedImplInstance<'db>,
    const_name: IdentId<'db>,
) -> Option<(Body<'db>, TyId<'db>, CompleteSubst<'db>)> {
    let explicit = match resolved.selected().origin(db) {
        ImplementorOrigin::Hir(impl_trait) => impl_trait
            .assoc_consts(db)
            .into_iter()
            .find(|constant| constant.name(db) == Some(const_name))
            .and_then(|constant| {
                Some((
                    constant.value_body(db)?,
                    constant.ty(db)?,
                    CompleteSubst::for_owner(
                        db,
                        impl_trait.into(),
                        resolved.impl_args(db).to_vec(),
                    )
                    .ok()?,
                ))
            }),
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => None,
        ImplementorOrigin::Assumption => return None,
    };
    explicit.or_else(|| {
        let inst = resolved.trait_inst();
        let trait_ = inst.def(db);
        let constant = trait_.const_(db, const_name)?;
        Some((
            constant.default_body(db)?,
            constant.ty(db)?,
            CompleteSubst::for_owner(db, trait_.into(), inst.args(db).to_vec()).ok()?,
        ))
    })
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, Update)]
pub(crate) struct TraitEnv<'db> {
    /// Syntax-only impl candidates grouped by trait definition.
    pub(crate) impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>>,

    /// Syntax-only impl candidates grouped by their nominal self-type root.
    impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>>,

    /// Blanket, alias-rooted, projection-rooted, or otherwise unindexable impls
    /// that must be considered for every receiver type.
    unindexed_self_impls: Vec<ImplTrait<'db>>,

    /// Explicit aliases remain separate from all ordinary discovery indexes.
    named_by_name: FxHashMap<IdentId<'db>, Vec<ImplTrait<'db>>>,

    ingot: Ingot<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        let mut impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>> = FxHashMap::default();
        let mut impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>> =
            FxHashMap::default();
        let mut unindexed_self_impls = Vec::new();
        let mut named_by_name: FxHashMap<IdentId<'db>, Vec<ImplTrait<'db>>> = FxHashMap::default();
        for &item in ingot.all_impl_traits(db).iter() {
            if let ImplSelectionClass::Named(name) = impl_selection_class(db, item) {
                named_by_name.entry(name).or_default().push(item);
            }
        }

        for impl_map in ingot
            .resolved_external_ingots(db)
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // Raw collection is syntax-only. Candidate lookup performs semantic
            // lowering after this environment has been fully assembled.
            for (trait_def, implementors) in &impl_map.by_trait {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied().filter(|&item| {
                        impl_selection_class(db, item) == ImplSelectionClass::Ordinary
                    }));
            }
            for (self_key, implementors) in &impl_map.by_self {
                impls_by_self.entry(*self_key).or_default().extend(
                    implementors.iter().copied().filter(|&item| {
                        impl_selection_class(db, item) == ImplSelectionClass::Ordinary
                    }),
                );
            }
            unindexed_self_impls.extend(
                impl_map
                    .unindexed_self
                    .iter()
                    .copied()
                    .filter(|&item| impl_selection_class(db, item) == ImplSelectionClass::Ordinary),
            );
        }

        Self {
            impls,
            impls_by_self,
            unindexed_self_impls,
            named_by_name,
            ingot,
        }
    }

    fn impls_for_self_key(
        &self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Vec<ImplementorId<'db>> {
        let key = match ty.data(db) {
            TyData::TyBase(TyBase::Prim(prim)) => Some(ImplSelfKey::Prim(*prim)),
            TyData::TyBase(TyBase::Adt(adt)) => Some(ImplSelfKey::Item(adt.scope(db))),
            TyData::TyBase(TyBase::Contract(contract)) => Some(ImplSelfKey::Item(contract.scope())),
            _ => None,
        };
        key.and_then(|key| self.impls_by_self.get(&key))
            .into_iter()
            .flatten()
            .chain(&self.unindexed_self_impls)
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }

    fn impls_for_trait(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_def: Trait<'db>,
    ) -> Vec<ImplementorId<'db>> {
        self.impls
            .get(&trait_def)
            .into_iter()
            .flatten()
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }
}

/// Represents a slim, internal view of a trait impl, derived from an
/// `ImplTrait` item and its lowered trait instance.
#[salsa::interned]
#[derive(Debug)]
pub struct ImplementorId<'db> {
    /// The trait instance that this impl realizes.
    pub(crate) trait_: TraitInstId<'db>,

    /// The type parameters of this implementor.
    #[return_ref]
    pub(crate) params: Vec<TyId<'db>>,

    #[return_ref]
    pub(crate) types: IndexMap<IdentId<'db>, TyId<'db>>,

    pub(crate) origin: ImplementorOrigin<'db>,
}

impl<'db> ImplementorId<'db> {
    pub(crate) fn assumption(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> Self {
        ImplementorId::new(
            db,
            inst,
            Vec::new(),
            IndexMap::new(),
            ImplementorOrigin::Assumption,
        )
    }

    pub(crate) fn hir_impl_trait(self, db: &'db dyn HirAnalysisDb) -> ImplTrait<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => impl_trait,
            ImplementorOrigin::VirtualContract(contract) => panic!(
                "requested HIR impl-trait for virtual implementor (contract={})",
                contract
                    .name(db)
                    .to_opt()
                    .map_or_else(|| "<unknown>".to_string(), |n| n.data(db).to_string())
            ),
            ImplementorOrigin::Assumption => {
                panic!("requested HIR impl-trait for assumption-based implementor")
            }
            ImplementorOrigin::Callable => {
                panic!("requested HIR impl-trait for a callable value's builtin implementor")
            }
            ImplementorOrigin::Structural => {
                panic!("requested HIR impl-trait for a structural builtin implementor")
            }
        }
    }

    /// Associated type defined in this impl, if any.
    pub(crate) fn assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.types(db).get(&name).copied()
    }

    /// Trait definition implemented by this impl.
    pub(crate) fn trait_def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.trait_(db).def(db)
    }

    /// Semantic self type of this impl.
    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// Trait instance realized by this impl, including its associated type definitions.
    pub(crate) fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        let trait_inst = self.trait_(db);
        if self.types(db).is_empty() {
            return trait_inst;
        }

        let mut assoc_type_bindings = trait_inst.assoc_type_bindings(db).clone();
        for (name, ty) in self.types(db) {
            assoc_type_bindings.insert(*name, *ty);
        }

        TraitInstId::new(
            db,
            trait_inst.def(db),
            trait_inst.args(db).to_vec(),
            assoc_type_bindings,
            trait_inst.mode(db),
        )
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => {
                collect_candidate_constraints(db, impl_trait.into())
                    .instantiate(db, self.params(db))
            }
            ImplementorOrigin::VirtualContract(_)
            | ImplementorOrigin::Assumption
            | ImplementorOrigin::Callable
            | ImplementorOrigin::Structural => PredicateListId::empty_list(db),
        }
    }

    /// Method map for this impl, keyed by name: for a method with one body
    /// per mode, its `ref` body.
    pub(crate) fn methods(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexMap<IdentId<'db>, Func<'db>> {
        &collect_implementor_methods(db, self).methods
    }

    /// The impl's body of the method `name` at `mode`: the `mut` body of a
    /// method with one body per mode at `mut`, and its method otherwise.
    pub(crate) fn method_at(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        mode: Option<BorrowKind>,
    ) -> Option<Func<'db>> {
        let methods = collect_implementor_methods(db, self);
        mode.filter(|&mode| mode == BorrowKind::Mut)
            .and_then(|_| methods.mut_bodies.get(&name))
            .or_else(|| methods.methods.get(&name))
            .copied()
    }

    /// Whether the implementation gives the trait's `mut?` method `trait_m`,
    /// named `name`, its `mut` mode: by a `mut?` method, a `mut` body, or
    /// the trait method's default body.
    fn gives_mut_mode(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        trait_m: Func<'db>,
    ) -> bool {
        let methods = collect_implementor_methods(db, self);
        methods.mut_bodies.contains_key(&name)
            || methods
                .methods
                .get(&name)
                .map_or(trait_m.body(db).is_some(), |method| method.is_maybe_mut(db))
    }

    /// Whether the implementation gives the trait's `mut?` methods their
    /// `mut` modes, so `T: mut Trait` holds by it under its `mut?` bounds:
    /// each such method is `mut?` or keeps its `mut?` default body. A
    /// compiler-made implementor's trait has no `mut?` methods.
    pub(crate) fn gives_mut(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.trait_def(db)
            .method_defs(db)
            .iter()
            .filter(|(_, method)| method.is_maybe_mut(db))
            .all(|(&name, &method)| self.gives_mut_mode(db, name, method))
    }

    /// Compare impl methods vs. trait methods and report missing/mismatched ones.
    pub(crate) fn diags_method_conformance(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        if !matches!(self.origin(db), ImplementorOrigin::Hir(_)) {
            return Vec::new();
        }
        let mut diags = vec![];
        let impl_methods = self.methods(db);
        let hir_trait = self.trait_def(db);
        let trait_methods = self.trait_def(db).method_defs(db);
        let mut required_methods: IndexSet<_> = trait_methods
            .iter()
            .filter_map(|(name, &trait_method)| trait_method.body(db).is_none().then_some(*name))
            .collect();

        let methods = collect_implementor_methods(db, self);
        for (first, other) in &methods.conflicts {
            diags.push(
                ImplDiag::ConflictMethodImpl {
                    primary: other.as_callable(db).unwrap(),
                    conflict_with: first.as_callable(db).unwrap(),
                }
                .into(),
            );
        }
        for (name, impl_m) in impl_methods {
            let Some(trait_m) = trait_methods.get(name) else {
                diags.push(
                    ImplDiag::MethodNotDefinedInTrait {
                        primary: self.hir_impl_trait(db).span().trait_ref().into(),
                        method_name: *name,
                        trait_: hir_trait,
                    }
                    .into(),
                );
                continue;
            };
            let trait_m = trait_m.as_callable(db).unwrap();
            compare_impl_method(
                db,
                impl_m.as_callable(db).unwrap(),
                trait_m,
                self.trait_inst(db),
                BorrowKind::Ref,
                &mut diags,
            );
            if let Some(mut_body) = methods.mut_bodies.get(name) {
                compare_impl_method(
                    db,
                    mut_body.as_callable(db).unwrap(),
                    trait_m,
                    self.trait_inst(db),
                    BorrowKind::Mut,
                    &mut diags,
                );
            }
            required_methods.remove(name);
        }

        // Either every `mut?` method of the trait has its `mut` mode here, or
        // none has.
        let modes: Vec<_> = trait_methods
            .iter()
            .filter(|(_, method)| method.is_maybe_mut(db))
            .map(|(&name, &method)| (name, self.gives_mut_mode(db, name, method)))
            .collect();
        if let Some(&(given, _)) = modes.iter().find(|(_, gives)| *gives)
            && let Some(&(missing, _)) = modes.iter().find(|(_, gives)| !gives)
        {
            diags.push(
                ImplDiag::PartialMutImpl {
                    primary: self.hir_impl_trait(db).span().trait_ref().into(),
                    given,
                    missing,
                }
                .into(),
            );
        }

        if !required_methods.is_empty() {
            diags.push(
                ImplDiag::NotAllTraitItemsImplemented {
                    primary: self.hir_impl_trait(db).span().ty().into(),
                    not_implemented: required_methods.into_iter().collect(),
                }
                .into(),
            );
        }

        diags
    }
}

/// Returns `true` if the given two implementors conflict.
///
/// This mirrors the legacy `Implementor`-based semantics:
/// - instantiate both implementors with fresh vars and unify them;
/// - then check that the merged constraints are satisfiable.
pub(crate) fn does_impl_trait_conflict<'db>(
    db: &'db dyn HirAnalysisDb,
    a: ImplementorId<'db>,
    b: ImplementorId<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let a = table.instantiate_with_fresh_vars(a);
    let b = table.instantiate_with_fresh_vars(b);

    if table.unify(a, b).is_err() {
        return false;
    }

    let a_constraints = a.constraints(db);
    let b_constraints = b.constraints(db);

    if a_constraints.is_empty(db) && b_constraints.is_empty(db) {
        return true;
    }

    // Check if all constraints from both implementations would be satisfiable
    // when the types are unified.
    let merged_constraints = a_constraints.merge(db, b_constraints);
    let solve_cx = TraitSolveCx::new(db, a.trait_def(db).scope())
        .with_assumptions(PredicateListId::empty_list(db));

    for &constraint in merged_constraints.list(db) {
        let constraint = constraint.fold_with(db, &mut table);

        match is_goal_satisfiable(db, solve_cx, constraint) {
            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid => {
                return false;
            }
            _ => {
                // Constraint is satisfiable or needs more information, continue checking.
            }
        }
    }

    true
}

/// The positional identity of a trait application. Associated equalities live
/// in predicates and resolution evidence, not in a projection's identity.
#[salsa::interned]
#[derive(Debug)]
pub struct TraitRefId<'db> {
    pub key: Trait<'db>,
    #[return_ref]
    pub args: Vec<TyId<'db>>,
}

impl<'db> TraitRefId<'db> {
    pub fn def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.key(db)
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }

    pub fn as_predicate(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        TraitInstId::new_simple(db, self.def(db), self.args(db).to_vec())
    }

    pub fn project_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.def(db)
            .assoc_ty(db, name)
            .map(|_| TyId::assoc_ty(db, self, name))
    }
}

/// A trait predicate, including associated equality evidence when present.
#[salsa::interned]
#[derive(Debug)]
pub struct TraitInstId<'db> {
    pub key: Trait<'db>,
    /// Regular type and const parameters: [Self, ExplicitTypeParam1, ..., ExplicitConstParamN]
    #[return_ref]
    pub args: Vec<TyId<'db>>,

    /// Associated type bounds specified by user, eg `Iterator<Item=i32>`
    #[return_ref]
    pub assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>>,

    /// The modes the predicate grants the trait's `mut?` methods at:
    /// `T: Trait` grants `ref`, `T: mut Trait` both. `mut?` (`T: mut? Trait`)
    /// is `mut` where the item it bounds is used at `mut`, and stands only
    /// in an item's declared bounds.
    pub mode: ModeMarker,
}

impl<'db> TraitInstId<'db> {
    pub fn def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.key(db)
    }

    pub fn new_simple(db: &'db dyn HirAnalysisDb, def: Trait<'db>, args: Vec<TyId<'db>>) -> Self {
        Self::new(db, def, args, IndexMap::new(), ModeMarker::None)
    }

    /// The predicate with `mode` in place of its own.
    pub fn with_mode(self, db: &'db dyn HirAnalysisDb, mode: ModeMarker) -> Self {
        Self::new(
            db,
            self.def(db),
            self.args(db).clone(),
            self.assoc_type_bindings(db).clone(),
            mode,
        )
    }

    /// The predicate a bound of an item used at `mode` is: a `mut?` bound is
    /// `mut` at `mut`, and plain otherwise.
    pub fn at_mode(self, db: &'db dyn HirAnalysisDb, mode: Option<BorrowKind>) -> Self {
        match (self.mode(db), mode) {
            (ModeMarker::MaybeMut, Some(BorrowKind::Mut)) => self.with_mode(db, ModeMarker::Mut),
            (ModeMarker::MaybeMut, _) => self.with_mode(db, ModeMarker::None),
            _ => self,
        }
    }

    /// The goal that the predicate's self type grants the trait's `mut?`
    /// methods at `mut`: `T: mut Trait`, which the predicate's associated
    /// equalities, checked with it, need not repeat.
    pub fn mut_goal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(
            db,
            self.def(db),
            self.args(db).clone(),
            IndexMap::new(),
            ModeMarker::Mut,
        )
    }

    /// Whether the predicate grants the `mut` modes of the trait's `mut?`
    /// methods.
    pub fn is_mut(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.mode(db) == ModeMarker::Mut
    }

    /// Extract positional identity only. The caller remains responsible for
    /// preserving this predicate's associated equalities as evidence.
    pub fn trait_ref(self, db: &'db dyn HirAnalysisDb) -> TraitRefId<'db> {
        TraitRefId::new(db, self.def(db), self.args(db).to_vec())
    }

    pub fn with_fresh_vars(
        db: &'db dyn HirAnalysisDb,
        def: Trait<'db>,
        table: &mut UnificationTable<'db>,
    ) -> Self {
        let args = def
            .params(db)
            .iter()
            .map(|ty| table.new_var_from_param(*ty))
            .collect::<Vec<_>>();
        Self::new_simple(db, def, args)
    }

    pub fn assoc_ty_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<(IdentId<'db>, TyId<'db>)> {
        self.assoc_type_bindings(db)
            .iter()
            .map(|(&name, &ty)| (name, ty))
            .collect()
    }

    pub fn bound_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.assoc_type_bindings(db).get(&name).copied()
    }

    pub fn project_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.trait_ref(db).project_assoc_ty(db, name)
    }

    /// Normalize arguments of this trait instance.
    pub(crate) fn normalize(
        self,
        db: &'db dyn HirAnalysisDb,
        scope: crate::core::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let normalized_args: Vec<_> = self
            .args(db)
            .iter()
            .map(|&arg| crate::analysis::ty::normalize::normalize_ty(db, arg, scope, assumptions))
            .collect();
        Self::new(
            db,
            self.def(db),
            normalized_args,
            self.assoc_type_bindings(db).clone(),
            self.mode(db),
        )
    }

    pub fn pretty_print(self, db: &dyn HirAnalysisDb, as_pred: bool) -> String {
        if as_pred {
            let inst = self.pretty_print(db, false);
            let self_ty = self.self_ty(db);
            format!("{}: {}", self_ty.pretty_print(db), inst)
        } else if let Some(shape) = CallableShape::of(db, self.def(db)) {
            // A callable shape prints as the sugar that names it,
            // `Fn(own A, B) -> R`; `-> _` when the result is left open.
            let args = &self.args(db)[1..];
            let (params, ret) = match args.split_last() {
                Some((ret, params)) if shape.result_is_arg() => (params, Some(*ret)),
                _ => (
                    args,
                    self.assoc_type_bindings(db)
                        .get(&IdentId::new(db, "Out".to_string()))
                        .copied(),
                ),
            };
            let params: Vec<_> = shape
                .modes
                .iter()
                .zip(params)
                .map(|(mode, ty)| {
                    let ty = ty.pretty_print(db);
                    match mode {
                        FuncParamMode::Own => format!("own {ty}"),
                        FuncParamMode::Mut => format!("mut {ty}"),
                        FuncParamMode::View => ty.clone(),
                    }
                })
                .collect();
            let name = if shape.mut_receiver { "FnMut" } else { "Fn" };
            let ret = match ret {
                Some(ret) if ret == TyId::unit(db) => String::new(),
                Some(ret) => format!(" -> {}", ret.pretty_print(db)),
                None => " -> _".to_string(),
            };
            format!("{name}({}){ret}", params.join(", "))
        } else {
            let mut s = self.mode(db).prefix().to_string();
            s.push_str(
                self.def(db)
                    .name(db)
                    .to_opt()
                    .map_or("<unknown>", |n| n.data(db).as_str()),
            );

            let mut args = self.args(db).iter().map(|ty| ty.pretty_print(db));
            // Skip the first type parameter since it's the implementor type.
            args.next();

            let mut has_generics = false;
            if let Some(first) = args.next() {
                s.push('<');
                s.push_str(first);
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg);
                }
                has_generics = true;
            }

            // Add associated type bindings
            if !self.assoc_type_bindings(db).is_empty() {
                if !has_generics {
                    s.push('<');
                } else {
                    s.push_str(", ");
                }

                let mut first_assoc = true;
                for (name, ty) in self.assoc_type_bindings(db) {
                    if !first_assoc {
                        s.push_str(", ");
                    }
                    first_assoc = false;
                    s.push_str(name.data(db));
                    s.push_str(" = ");
                    s.push_str(ty.pretty_print(db));
                }
                has_generics = true;
            }

            if has_generics {
                s.push('>');
            }

            s
        }
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }
}

// Represents a trait definition.
// (TraitDef struct and impl removed)

// (TraitMethod struct and impl removed)

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use common::indexmap::IndexMap;

    use super::{TraitInstId, impls_for_trait_def, resolve_trait_impl_instance};
    use crate::analysis::ty::trait_resolution::{Selection, TraitSolveCx};
    use crate::hir_def::{IdentId, ItemKind};
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn trait_env_collects_overlapping_constrained_impls_without_cycle() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("trait_env_collects_overlapping_constrained_impls_without_cycle.fe"),
            r#"
trait Marker {}
trait Foo {}

impl<T> Foo for T where T: Marker {}
impl<T> Foo for T where T: Marker {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Foo") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Foo` trait");

        let impls = impls_for_trait_def(&db, top_mod.ingot(&db), trait_);
        assert_eq!(impls.len(), 2);
    }

    #[test]
    fn resolved_impl_instance_preserves_default_assoc_template_and_concrete_args() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "resolved_impl_instance_preserves_default_assoc_template_and_concrete_args.fe",
            ),
            r#"
trait Projects<T> {
    type Output = T
}

struct Wrapper<T> {}

impl<T> Projects<T> for Wrapper<T> {}

fn probe(value: own Wrapper<u16>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_) => Some(trait_),
                _ => None,
            })
            .expect("missing Projects trait");
        let func = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func),
                _ => None,
            })
            .expect("missing probe function");
        let self_ty = func
            .params(&db)
            .next()
            .expect("missing probe parameter")
            .ty(&db);
        let projected_arg = self_ty.generic_args(&db)[0];
        let inst = TraitInstId::new(
            &db,
            trait_,
            vec![self_ty, projected_arg],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let Selection::Unique(resolved) = resolve_trait_impl_instance(
            &db,
            TraitSolveCx::new(&db, func.scope()),
            inst,
            crate::analysis::ty::trait_choice::OrdinarySelection::for_tests(),
        ) else {
            panic!("expected one concrete Projects implementation");
        };
        let output = IdentId::new(&db, "Output");
        let template = resolved
            .assoc_ty_template(&db, output)
            .expect("missing default associated type template");
        let instantiated = resolved
            .instantiated_assoc_ty(&db, output)
            .expect("missing instantiated default associated type");

        assert!(template.has_param(&db));
        assert_eq!(instantiated.pretty_print(&db).to_string(), "u16");
        assert_eq!(resolved.impl_args(&db), &[instantiated]);
    }
}

#[cfg(test)]
mod retained_application_tests {
    use super::*;
    use crate::{
        analysis::ty::{
            adt_def::AdtRef,
            trait_resolution::constraint::collect_func_def_constraints,
            ty_def::{Kind, PrimTy, TyVarSort},
            ty_lower::collect_generic_params,
        },
        hir_def::{TopLevelMod, WhereClauseOwner},
        test_db::{HirAnalysisTestDb, find_func},
    };

    fn trait_named<'db>(
        db: &'db HirAnalysisTestDb,
        module: TopLevelMod<'db>,
        name: &str,
    ) -> Trait<'db> {
        *module
            .all_traits(db)
            .iter()
            .find(|t| t.name(db).to_opt().unwrap().data(db) == name)
            .unwrap()
    }
    fn record<'db>(db: &'db HirAnalysisTestDb, module: TopLevelMod<'db>, name: &str) -> TyId<'db> {
        let item = *module
            .all_structs(db)
            .iter()
            .find(|s| s.name(db).to_opt().unwrap().data(db) == name)
            .unwrap();
        TyId::adt(db, AdtRef::from(item).as_adt(db))
    }
    fn candidates<'db>(
        db: &'db HirAnalysisTestDb,
        module: TopLevelMod<'db>,
        name: &str,
    ) -> Vec<ImplementorId<'db>> {
        impls_for_trait_def(db, module.ingot(db), trait_named(db, module, name)).to_vec()
    }
    fn goal<'db>(
        db: &'db HirAnalysisTestDb,
        module: TopLevelMod<'db>,
        name: &str,
        args: Vec<TyId<'db>>,
    ) -> TraitInstId<'db> {
        TraitInstId::new_simple(db, trait_named(db, module, name), args)
    }
    fn out(db: &HirAnalysisTestDb) -> IdentId<'_> {
        IdentId::new(db, "Out")
    }

    #[test]
    fn proof_checks_prerequisites_before_header_reconstruction() {
        let mut db = HirAnalysisTestDb::default();
        let file=db.new_stand_alone("retained_prerequisites.fe".into(),"trait Marker {}\ntrait Pick {}\nstruct Yes {}\nstruct No {}\nimpl Marker for Yes {}\nimpl<T> Pick for T where T: Marker {}");
        let (module, _) = db.top_mod(file);
        let candidate = candidates(&db, module, "Pick")[0];
        let cx = TraitSolveCx::new(&db, module.scope());
        let bad = goal(&db, module, "Pick", vec![record(&db, module, "No")]);
        assert!(
            instantiate_selected_impl(&db, candidate, bad).is_some(),
            "matching a header is not proof"
        );
        assert_eq!(
            prove_selected_application(&db, cx, candidate, bad).unwrap_err(),
            CheckedApplicationError::NotProven
        );
        let good = goal(&db, module, "Pick", vec![record(&db, module, "Yes")]);
        let checked = prove_selected_application(&db, cx, candidate, good).unwrap();
        assert_eq!(
            checked.resolved().selected().origin(&db),
            candidate.origin(&db)
        );
        assert_eq!(checked.origin_context().scope(), module.scope());
    }

    #[test]
    fn full_trait_arguments_equalities_generic_output_and_method_readers() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "retained_outputs.fe".into(),
            r#"
trait Pick<Tag> {
    type Out
    fn implemented(self) -> u256
    fn inherited(self) -> u256 { 8 }
}
struct Wrap<T> {}
impl<T> Pick<bool> for Wrap<T> {
    type Out = T
    fn implemented(self) -> u256 { 1 }
}
"#,
        );
        let (module, _) = db.top_mod(file);
        let candidate = candidates(&db, module, "Pick")[0];
        let cx = TraitSolveCx::new(&db, module.scope());
        let u32_ = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U32)));
        let bool_ = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Bool)));
        let subject = TyId::app(&db, record(&db, module, "Wrap"), u32_);
        let g = goal(&db, module, "Pick", vec![subject, bool_]);
        let checked = prove_selected_application(&db, cx, candidate, g).unwrap();
        assert_eq!(checked.associated_type(out(&db)), Some(u32_));
        assert_eq!(checked.resolved().impl_args(&db), [u32_]);
        let Selection::Unique(own) = checked.method(IdentId::new(&db, "implemented")) else {
            panic!("explicit method")
        };
        assert_eq!(own.body_args(), [u32_]);
        let Selection::Unique(default) = checked.method(IdentId::new(&db, "inherited")) else {
            panic!("default method")
        };
        assert_eq!(default.body_args(), g.args(&db));
        let eq = TraitInstId::new(
            &db,
            g.def(&db),
            g.args(&db).to_vec(),
            [(out(&db), u32_)].into_iter().collect::<IndexMap<_, _>>(),
        );
        assert_eq!(
            checked
                .revalidate(cx, eq)
                .unwrap()
                .associated_type(out(&db)),
            Some(u32_)
        );
        let wrong = TraitInstId::new(
            &db,
            g.def(&db),
            g.args(&db).to_vec(),
            [(out(&db), bool_)].into_iter().collect::<IndexMap<_, _>>(),
        );
        assert!(
            checked.revalidate(cx, wrong).is_err(),
            "same trait/subject slot does not prove Out equality"
        );
        assert!(
            checked
                .revalidate(cx, goal(&db, module, "Pick", vec![subject, u32_]))
                .is_err()
        );
    }

    #[test]
    fn destination_proves_its_own_premises_and_does_not_switch_candidate() {
        let mut db = HirAnalysisTestDb::default();
        // Intentional overlapping solver fixture: does not certify source coherence.
        let file = db.new_stand_alone(
            "retained_destination.fe".into(),
            r#"
trait Marker {} trait Unused {} trait Pick { type Out }
struct Wrap<T> {}
impl<T> Pick for Wrap<T> where T: Marker { type Out = T }
impl<T> Pick for Wrap<T> { type Out = bool }
fn probe<T: Marker + Unused>(_ value: T) {}
"#,
        );
        let (module, _) = db.top_mod(file);
        let probe = find_func(&db, module, "probe");
        let t = collect_generic_params(&db, probe.into()).explicit_params(&db)[0];
        let premises = collect_func_def_constraints(&db, probe.into(), true).instantiate_identity();
        let origin = TraitSolveCx::new(&db, probe.scope()).with_assumptions(premises);
        let candidate = candidates(&db, module, "Pick")[0];
        let g = goal(
            &db,
            module,
            "Pick",
            vec![TyId::app(&db, record(&db, module, "Wrap"), t)],
        );
        let checked = prove_selected_application(&db, origin, candidate, g).unwrap();
        assert_eq!(checked.associated_type(out(&db)), Some(t));
        let marker = goal(&db, module, "Marker", vec![t]);
        let required = TraitSolveCx::new(&db, probe.scope())
            .with_assumptions(PredicateListId::new(&db, vec![marker]));
        assert!(
            checked.revalidate(required, g).is_ok(),
            "unused origin assumptions may be removed"
        );
        let empty = TraitSolveCx::new(&db, probe.scope());
        assert_eq!(
            checked.revalidate(empty, g).unwrap_err(),
            CheckedApplicationError::NotProven
        );
        assert!(
            matches!(resolve_trait_impl_instance(&db,empty,g,crate::analysis::ty::trait_choice::OrdinarySelection::for_tests()),Selection::Unique(other) if other.selected().origin(&db)!=candidate.origin(&db)),
            "ordinary discovery can find the fallback, but retained choice cannot switch"
        );
        assert_eq!(checked.origin_context().scope(), probe.scope());
    }

    #[test]
    fn fresh_variables_assumptions_and_unconstrained_impl_arguments_are_not_certificates() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "retained_unknown.fe".into(),
            "trait Pick {}\nstruct Key {}\nimpl<T> Pick for Key {}",
        );
        let (module, _) = db.top_mod(file);
        let candidate = candidates(&db, module, "Pick")[0];
        let cx = TraitSolveCx::new(&db, module.scope());
        let g = goal(&db, module, "Pick", vec![record(&db, module, "Key")]);
        assert_eq!(
            prove_selected_application(&db, cx, candidate, g).unwrap_err(),
            CheckedApplicationError::UnresolvedApplication
        );
        let assumption = ImplementorId::assumption(&db, g);
        assert_eq!(
            prove_selected_application(&db, cx, assumption, g).unwrap_err(),
            CheckedApplicationError::UnsupportedInput
        );
        let mut table = UnificationTable::new(&db);
        let unknown = table.new_var(TyVarSort::General, &Kind::Star);
        let unknown = goal(&db, module, "Pick", vec![unknown]);
        assert_eq!(
            prove_selected_application(&db, cx, candidate, unknown).unwrap_err(),
            CheckedApplicationError::UnsupportedInput
        );
    }

    #[test]
    fn root_and_nested_const_conditions_follow_ordinary_admission() {
        for condition in ["true", "false", "7", "fail()"] {
            let mut db = HirAnalysisTestDb::default();
            let source = format!(
                "const fn fail() -> bool {{ assert!(false)\ntrue }}\ntrait Marker {{}}\ntrait Lift {{}}\ntrait Twice {{}}\nstruct Key {{}}\nimpl Marker for Key where {{ {condition} }} {{}}\nimpl<T> Lift for T where T: Marker {{}}\nimpl<T> Twice for T where T: Lift {{}}"
            );
            let file = db.new_stand_alone("retained_conditions.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            let cx = TraitSolveCx::new(&db, module.scope());
            let key = record(&db, module, "Key");
            for name in ["Marker", "Lift", "Twice"] {
                let candidate = candidates(&db, module, name)[0];
                let g = goal(&db, module, name, vec![key]);
                assert!(
                    matches!(cx.restricted_goal(&db,g,candidate),GoalSatisfiability::Satisfied(solution) if !solution.value.impl_conditions.instances(&db).is_empty()),
                    "a structural proof must retain nested condition metadata"
                );
                let checked = prove_selected_application(&db, cx, candidate, g);
                assert_eq!(
                    checked.is_ok(),
                    condition == "true",
                    "root/nested conditions cannot be ignored"
                );
                if condition != "true" {
                    assert_eq!(
                        checked.unwrap_err(),
                        CheckedApplicationError::ConditionsNotAdmitted
                    );
                }
            }
            let marker = candidates(&db, module, "Marker")[0].hir_impl_trait(&db);
            let ordinary = super::super::ty_check::check_where_const_predicates(
                &db,
                WhereClauseOwner::ImplTrait(marker),
            );
            assert_eq!(
                ordinary.is_empty(),
                condition == "true",
                "ordinary ground condition admission remains master's policy"
            );
        }
    }

    #[test]
    fn unresolved_nested_associated_outputs_are_not_exposed() {
        let mut db = HirAnalysisTestDb::default();
        let file=db.new_stand_alone("retained_projection.fe".into(),"trait Marker { type Out }\ntrait Pick { type Out }\nstruct Box<T> {}\nstruct Wrap<T> {}\nfn probe<T: Marker>(_ value: T) {}\nimpl<T> Pick for Wrap<T> where T: Marker { type Out = Box<T::Out> }");
        let (module, _) = db.top_mod(file);
        let probe = find_func(&db, module, "probe");
        let t = collect_generic_params(&db, probe.into()).explicit_params(&db)[0];
        let premises = collect_func_def_constraints(&db, probe.into(), true).instantiate_identity();
        let cx = TraitSolveCx::new(&db, probe.scope()).with_assumptions(premises);
        let candidate = candidates(&db, module, "Pick")[0];
        let g = goal(
            &db,
            module,
            "Pick",
            vec![TyId::app(&db, record(&db, module, "Wrap"), t)],
        );
        assert!(matches!(
            cx.restricted_goal(&db, g, candidate),
            GoalSatisfiability::Satisfied(_)
        ));
        assert_eq!(
            prove_selected_application(&db, cx, candidate, g).unwrap_err(),
            CheckedApplicationError::UnresolvedApplication
        );
    }

    #[test]
    fn warm_edits_reconstruct_current_outputs_instead_of_serializing_a_choice() {
        use salsa::Setter;
        fn read(db: &HirAnalysisTestDb, file: common::file::File) -> String {
            let (module, _) = db.top_mod(file);
            let candidate = candidates(db, module, "Pick")[0];
            let cx = TraitSolveCx::new(db, module.scope());
            let g = goal(db, module, "Pick", vec![record(db, module, "Key")]);
            prove_selected_application(db, cx, candidate, g)
                .unwrap()
                .associated_type(out(db))
                .unwrap()
                .pretty_print(db)
                .to_string()
        }
        let source = "trait Pick { type Out }\nstruct Key {}\nstruct A {}\nstruct B {}\nimpl Pick for Key { type Out = A }";
        let mut warm = HirAnalysisTestDb::default();
        let file = warm.new_stand_alone("retained_warm.fe".into(), source);
        assert_eq!(read(&warm, file), "A");
        assert_eq!(read(&warm, file), "A");
        let changed = source.replace("type Out = A", "type Out = B");
        file.set_text(&mut warm).to(changed.clone());
        assert_eq!(read(&warm, file), "B");
        let mut fresh = HirAnalysisTestDb::default();
        let fresh_file = fresh.new_stand_alone("retained_warm.fe".into(), &changed);
        assert_eq!(read(&fresh, fresh_file), read(&warm, file));
    }
}

#[cfg(test)]
mod retained_application_tests_completion {
    use super::*;
    use crate::{analysis::ty::adt_def::AdtRef, test_db::HirAnalysisTestDb};
    #[test]
    fn recursive_failure_and_incomplete_growth_are_distinct() {
        for (bound, expected) in [
            ("T: Pick", CheckedApplicationError::NotProven),
            ("Wrap<T>: Pick", CheckedApplicationError::Incomplete),
        ] {
            let mut db = HirAnalysisTestDb::default();
            let source = format!(
                "trait Pick {{}}\nstruct Key {{}}\nstruct Wrap<T> {{}}\nimpl<T> Pick for T where {bound} {{}}"
            );
            let file = db.new_stand_alone("retained_completion.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            let trait_ = module.all_traits(&db)[0];
            let candidate = impls_for_trait_def(&db, module.ingot(&db), trait_)[0];
            let key = TyId::adt(&db, AdtRef::from(module.all_structs(&db)[0]).as_adt(&db));
            let goal = TraitInstId::new_simple(&db, trait_, vec![key]);
            assert_eq!(
                prove_selected_application(
                    &db,
                    TraitSolveCx::new(&db, module.scope()),
                    candidate,
                    goal
                )
                .unwrap_err(),
                expected
            );
        }
    }
}

#[cfg(test)]
mod retained_application_tests_admission {
    use super::*;
    use crate::test_db::{HirAnalysisTestDb, find_func};
    #[test]
    fn concrete_use_cannot_admit_a_generic_impl_condition_rejected_on_master() {
        let mut db = HirAnalysisTestDb::default();
        let file=db.new_stand_alone("retained_generic_condition.fe".into(),"trait Marker {}\nstruct Wrap<const N: usize> {}\nimpl<const N: usize> Marker for Wrap<N> where { N > 0 } {}\nfn probe(_ value: Wrap<1>) {}");
        let (module, _) = db.top_mod(file);
        let trait_ = module.all_traits(&db)[0];
        let candidate = impls_for_trait_def(&db, module.ingot(&db), trait_)[0];
        let probe = find_func(&db, module, "probe");
        let subject = probe.params(&db).next().unwrap().ty(&db);
        let subject = subject.as_view(&db).unwrap_or(subject);
        let goal = TraitInstId::new_simple(&db, trait_, vec![subject]);
        let cx = TraitSolveCx::new(&db, probe.scope());
        assert!(matches!(
            cx.restricted_goal(&db, goal, candidate),
            GoalSatisfiability::Satisfied(_)
        ));
        assert_eq!(
            prove_selected_application(&db, cx, candidate, goal).unwrap_err(),
            CheckedApplicationError::ConditionsNotAdmitted
        );
    }
    #[test]
    fn conditional_type_headers_need_the_owned_checked_use_adapter() {
        let mut db = HirAnalysisTestDb::default();
        let file=db.new_stand_alone("retained_type_conditions.fe".into(),"trait Pick {}\nstruct Bounded<const N: usize> where { N > 0 } {}\nimpl Pick for Bounded<1> {}\nfn probe(_ value: Bounded<1>) {}");
        let (module, _) = db.top_mod(file);
        let trait_ = module.all_traits(&db)[0];
        let candidate = impls_for_trait_def(&db, module.ingot(&db), trait_)[0];
        let probe = find_func(&db, module, "probe");
        let subject = probe.params(&db).next().unwrap().ty(&db);
        let subject = subject.as_view(&db).unwrap_or(subject);
        let goal = TraitInstId::new_simple(&db, trait_, vec![subject]);
        let cx = TraitSolveCx::new(&db, probe.scope());
        assert_eq!(
            prove_selected_application(&db, cx, candidate, goal).unwrap_err(),
            CheckedApplicationError::UnsupportedConditions
        );
    }
}

#[cfg(test)]
mod retained_application_tests_transitive_header {
    use super::*;
    use crate::{analysis::ty::adt_def::AdtRef, test_db::HirAnalysisTestDb};
    #[test]
    fn a_transitive_conditional_type_header_cannot_become_a_checked_application() {
        let mut db = HirAnalysisTestDb::default();
        let file=db.new_stand_alone("retained_transitive_header.fe".into(),"trait Marker<A> {}\ntrait Lift {}\ntrait Pick {}\nstruct Key {}\nstruct Other {}\nstruct Bounded<const N: usize> where { N > 0 } {}\nimpl Marker<Bounded<0>> for Other {}\nimpl Lift for Other where Other: Marker<Bounded<0>> {}\nimpl Pick for Key where Other: Lift {}");
        let (module, _) = db.top_mod(file);
        let trait_ = *module
            .all_traits(&db)
            .iter()
            .find(|t| t.name(&db).to_opt().unwrap().data(&db) == "Pick")
            .unwrap();
        let candidate = impls_for_trait_def(&db, module.ingot(&db), trait_)[0];
        let key = TyId::adt(&db, AdtRef::from(module.all_structs(&db)[0]).as_adt(&db));
        let goal = TraitInstId::new_simple(&db, trait_, vec![key]);
        let cx = TraitSolveCx::new(&db, module.scope());
        assert!(
            prove_selected_application(&db, cx, candidate, goal).is_err(),
            "an inadmissible prerequisite header is not a checked proof"
        );
    }
}
