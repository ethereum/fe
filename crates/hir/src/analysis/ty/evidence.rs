//! Evidence: how a trait goal holds.
//!
//! The checker proves each trait goal a body uses once, in the body's
//! context, and records the proof. A proof is an implementation with a proof
//! of each of its conditions, or one of the body's assumptions, named by its
//! position in the body's assumption list.
//!
//! A call also proves the bounds its callee declares. The callee's instance
//! receives those proofs (`SemanticInstanceKey::evidence`) and puts them in
//! place of the assumptions its body's proofs name, so a call the body
//! dispatches through a bound reaches its implementation without selecting
//! it again.

use salsa::Update;

use super::{
    fold::{TyFoldable, TyFolder},
    method_cmp::trait_to_impl_param_subst,
    normalize::normalize_ty,
    subst::substitute_complete,
    trait_def::{
        ImplementorOrigin, ResolvedImplInstance, TraitInstId, resolve_trait_impl_instance,
    },
    trait_lower::lower_impl_trait,
    trait_resolution::{
        PredicateListId, Selection, TraitSolveCx, answering_assumption,
        constraint::collect_candidate_constraints, normalize_trait_inst_preserving_validity,
    },
    ty_check::{BodyOwner, func_signature},
    ty_def::{BorrowKind, TyFlags, TyId},
    ty_lower::CompleteSubst,
    visitor::{TyVisitable, TyVisitor, collect_flags},
};
use crate::{
    analysis::{HirAnalysisDb, semantic::GenericSubst},
    hir_def::{CallableDef, Func, ItemKind},
};

#[salsa::interned]
#[derive(Debug)]
pub struct EvidenceId<'db> {
    #[return_ref]
    pub data: Evidence<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum Evidence<'db> {
    /// An implementation applied to the goal, with a proof of each of its
    /// conditions in the order `ImplementorId::constraints` lists them. A
    /// compiler-provided implementation has none.
    Impl {
        resolved: ResolvedImplInstance<'db>,
        conds: Vec<EvidenceId<'db>>,
    },
    /// The assumption at this position in the extended assumption list of
    /// the body the goal was proved in.
    Assumption(u32),
}

impl<'db> EvidenceId<'db> {
    /// The implementation this proof applies to its goal; none for an
    /// assumption.
    pub fn selection(self, db: &'db dyn HirAnalysisDb) -> Option<ResolvedImplInstance<'db>> {
        match self.data(db) {
            Evidence::Impl { resolved, .. } => Some(*resolved),
            Evidence::Assumption(_) => None,
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of it; absent if `bound` has no proof of one.
    fn rebase(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Option<Self> {
        Some(match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index as usize)?,
            Evidence::Impl { resolved, conds } => EvidenceId::new(
                db,
                Evidence::Impl {
                    resolved: *resolved,
                    conds: conds
                        .iter()
                        .map(|cond| cond.rebase(db, bound))
                        .collect::<Option<_>>()?,
                },
            ),
        })
    }

    /// Whether this proof names no assumption: it holds in any body.
    pub fn is_closed(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_is_closed(db, self)
    }

    /// The proofs of an implementation's conditions; none for an assumption.
    pub fn conds(self, db: &'db dyn HirAnalysisDb) -> &'db [EvidenceId<'db>] {
        match self.data(db) {
            Evidence::Impl { conds, .. } => conds,
            Evidence::Assumption(_) => &[],
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of that assumption, where `bound` has one.
    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Self {
        match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index as usize).unwrap_or(self),
            Evidence::Impl { resolved, conds } => EvidenceId::new(
                db,
                Evidence::Impl {
                    resolved: *resolved,
                    conds: conds.iter().map(|cond| cond.bind(db, bound)).collect(),
                },
            ),
        }
    }
}

#[salsa::tracked]
fn evidence_is_closed<'db>(db: &'db dyn HirAnalysisDb, evidence: EvidenceId<'db>) -> bool {
    match evidence.data(db) {
        Evidence::Assumption(_) => false,
        Evidence::Impl { conds, .. } => conds.iter().all(|cond| cond.is_closed(db)),
    }
}

/// Proofs of the assumptions a body is checked under, one per entry of its
/// signature (`TypedBody::signature`), each absent where the body proves
/// that assumption itself.
#[salsa::interned]
#[derive(Debug)]
pub struct EvidenceList<'db> {
    #[return_ref]
    pub entries: Vec<Option<EvidenceId<'db>>>,
}

impl<'db> EvidenceList<'db> {
    /// A list that supplies no proofs.
    pub fn none(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }

    /// The list of `entries`, without the absent proofs that end it, so
    /// lists that supply the same proofs are equal.
    pub fn of(db: &'db dyn HirAnalysisDb, mut entries: Vec<Option<EvidenceId<'db>>>) -> Self {
        while entries.last().is_some_and(Option::is_none) {
            entries.pop();
        }
        Self::new(db, entries)
    }

    /// The proof of the assumption at `index`, if the list supplies one.
    pub fn get(self, db: &'db dyn HirAnalysisDb, index: usize) -> Option<EvidenceId<'db>> {
        self.entries(db).get(index).copied().flatten()
    }

    /// Each proof with the assumptions it names replaced by `bound`'s proofs.
    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Self {
        if bound.entries(db).is_empty() {
            return self;
        }
        Self::new(
            db,
            self.entries(db)
                .iter()
                .map(|entry| entry.map(|evidence| evidence.bind(db, bound)))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'db> TyFoldable<'db> for EvidenceList<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self::new(
            db,
            self.entries(db)
                .iter()
                .map(|entry| entry.map(|evidence| evidence.fold_with(db, folder)))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'db> TyVisitable<'db> for EvidenceList<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        for evidence in self.entries(visitor.db()).iter().flatten() {
            evidence.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for EvidenceId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self.data(db) {
            Evidence::Impl { resolved, conds } => EvidenceId::new(
                db,
                Evidence::Impl {
                    resolved: resolved.fold_with(db, folder),
                    conds: conds
                        .iter()
                        .map(|cond| cond.fold_with(db, folder))
                        .collect(),
                },
            ),
            Evidence::Assumption(_) => self,
        }
    }
}

impl<'db> TyVisitable<'db> for EvidenceId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        if let Evidence::Impl { resolved, conds } = self.data(visitor.db()) {
            resolved.visit_with(visitor);
            for cond in conds {
                cond.visit_with(visitor);
            }
        }
    }
}

/// Proves `goal` in `solve_cx`, the context of the body that uses it. A goal
/// or proof that still holds inference variables, such as a generic function
/// item's own, has no evidence: it is proved where the body is instantiated.
pub fn prove<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    // No assumption answers a closed goal, so every body shares its proof.
    let closed = !collect_flags(db, goal)
        .intersects(TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION);
    let solve_cx = if closed {
        solve_cx.with_assumptions(PredicateListId::empty_list(db))
    } else {
        solve_cx
    };
    prove_in(db, solve_cx, goal)
}

#[salsa::tracked(cycle_fn = prove_cycle_recover, cycle_initial = prove_cycle_initial)]
fn prove_in<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    let settled = |flags: TyFlags| !flags.intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID);
    if !settled(collect_flags(db, goal)) {
        return None;
    }
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, goal) else {
        return None;
    };
    if !settled(collect_flags(db, resolved)) {
        return None;
    }
    let evidence = match resolved.selected().origin(db) {
        ImplementorOrigin::Assumption => {
            Evidence::Assumption(answering_assumption(db, solve_cx, goal)? as u32)
        }
        ImplementorOrigin::Hir(_) => {
            // A `mut` goal's conditions are its implementation's `mut?`
            // bounds at `mut`.
            let mode = Some(if goal.is_mut(db) {
                BorrowKind::Mut
            } else {
                BorrowKind::Ref
            });
            Evidence::Impl {
                resolved: resolved.as_proof(db),
                conds: resolved
                    .instantiated_constraints(db)
                    .list(db)
                    .iter()
                    .map(|&cond| prove(db, solve_cx, cond.at_mode(db, mode)))
                    .collect::<Option<_>>()?,
            }
        }
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => Evidence::Impl {
            resolved: resolved.as_proof(db),
            conds: Vec::new(),
        },
    };
    Some(EvidenceId::new(db, evidence))
}

/// The implementation answering `inst` where a checked body is
/// instantiated: the one the checker's `evidence` applies, or, for a goal the
/// checker proved by an assumption (a bound of the body) or did not prove,
/// the one `solve_cx` selects.
pub(crate) fn instantiated_selection<'db>(
    db: &'db dyn HirAnalysisDb,
    evidence: Option<EvidenceId<'db>>,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    match evidence.and_then(|evidence| evidence.selection(db)) {
        // The proof records its goal bare; the use answers its own, as
        // selection resolves it. An instantiated body can expose projections
        // its checker could not resolve.
        Some(resolved) => {
            let scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
            let resolved = if collect_flags(db, resolved).contains(TyFlags::HAS_PROJECTION) {
                normalize_ty(db, resolved, scope, solve_cx.assumptions())
            } else {
                resolved
            };
            Some(resolved.answering(normalize_trait_inst_preserving_validity(
                db,
                inst,
                scope,
                solve_cx.assumptions(),
            )))
        }
        None => match resolve_trait_impl_instance(db, solve_cx, inst) {
            Selection::Unique(resolved) => Some(resolved),
            Selection::Ambiguous(_) | Selection::NotFound => None,
        },
    }
}

/// How the method `body` of an implementation proves, at `mode`, the
/// assumptions it is checked under (`func_signature`) from what a call of the
/// trait's method `nominal` supplies. Each proof names as its assumptions the
/// implementation's conditions, as a proof selecting the implementation
/// proves them, followed by the assumptions `nominal`'s body is checked
/// under, as the call proves them. An assumption that only follows from
/// these leaves its proof absent. Absent for a method of no implementation.
#[salsa::tracked(return_ref)]
pub(crate) fn impl_method_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Func<'db>,
    nominal: Func<'db>,
    mode: Option<BorrowKind>,
) -> Option<Vec<Option<EvidenceId<'db>>>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let implementor = lower_impl_trait(db, impl_trait)?;
    let conditions = collect_candidate_constraints(db, impl_trait.into()).instantiate_identity();
    let (subst, _, _) = trait_to_impl_param_subst(
        db,
        CallableDef::Func(body),
        CallableDef::Func(nominal),
        implementor.trait_inst(db),
    );
    let nominal_signature = substitute_complete(
        db,
        func_signature(db, nominal, BodyOwner::func_at(db, nominal, mode).mode()),
        &subst.residualize(db),
    )
    .ok()?;
    let basis = PredicateListId::new(
        db,
        conditions
            .list(db)
            .iter()
            .chain(nominal_signature.list(db))
            .copied()
            .collect::<Vec<_>>(),
    );
    // A proof names its assumptions by position in the extended basis.
    let extended = basis.extend_all_bounds(db);
    let to_basis = EvidenceList::of(
        db,
        extended
            .list(db)
            .iter()
            .map(|assumption| {
                let index = basis.list(db).iter().position(|pred| pred == assumption)?;
                Some(EvidenceId::new(db, Evidence::Assumption(index as u32)))
            })
            .collect::<Vec<_>>(),
    );
    let solve_cx = TraitSolveCx::new(db, body.scope()).with_assumptions(basis);
    Some(
        func_signature(db, body, BodyOwner::func_at(db, body, mode).mode())
            .list(db)
            .iter()
            .map(|&goal| prove(db, solve_cx, goal)?.rebase(db, to_basis))
            .collect(),
    )
}

/// The proofs a call supplies to the method `body`, of the implementation
/// its `selection` proof selects, of the assumptions `body` is checked under
/// at `mode`: from the selection's proofs of the implementation's conditions
/// and the call's proofs of `nominal`'s assumptions, `bound`. `args`
/// instantiates `body`.
pub(crate) fn impl_method_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    (body, nominal, mode): (Func<'db>, Func<'db>, Option<BorrowKind>),
    selection: EvidenceId<'db>,
    bound: EvidenceList<'db>,
    args: &[TyId<'db>],
) -> EvidenceList<'db> {
    impl_method_bound_evidence_query(
        db,
        (body, nominal, mode),
        selection,
        bound,
        GenericSubst::for_owner(db, body.into(), args.to_vec()),
    )
}

// Calls of one implementation's method with one substitution and one
// selection share their instance's evidence.
#[salsa::tracked]
fn impl_method_bound_evidence_query<'db>(
    db: &'db dyn HirAnalysisDb,
    (body, nominal, mode): (Func<'db>, Func<'db>, Option<BorrowKind>),
    selection: EvidenceId<'db>,
    bound: EvidenceList<'db>,
    subst: GenericSubst<'db>,
) -> EvidenceList<'db> {
    let (Some(proofs), Some(subst)) = (
        impl_method_evidence(db, body, nominal, mode),
        subst.mapping(db).as_ref(),
    ) else {
        return EvidenceList::none(db);
    };
    let supplied = EvidenceList::of(
        db,
        selection
            .conds(db)
            .iter()
            .map(|&cond| Some(cond))
            .chain(bound.entries(db).iter().copied())
            .collect::<Vec<_>>(),
    );
    EvidenceList::of(
        db,
        proofs
            .iter()
            .map(|proof| {
                substitute_complete(db, (*proof)?, subst)
                    .ok()?
                    .rebase(db, supplied)
            })
            .collect::<Vec<_>>(),
    )
}

/// The evidence an instance of `func` at `mode` with the closed arguments
/// `args` runs with: `supplied`'s closed proofs, and a proof in `solve_cx`
/// of each assumption it gives none of, or gives one naming an assumption of
/// a body the instance does not run in. Instances of one body with one
/// substitution so share their evidence.
pub(crate) fn closed_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    (func, mode): (Func<'db>, Option<BorrowKind>),
    args: &[TyId<'db>],
    supplied: EvidenceList<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> EvidenceList<'db> {
    let signature = func_signature(db, func, mode);
    let closed = |index| {
        supplied
            .get(db, index)
            .filter(|evidence| evidence.is_closed(db))
    };
    if (0..signature.list(db).len()).all(|index| closed(index).is_some()) {
        return supplied;
    }
    let Some(goals) = CompleteSubst::for_owner(db, func.into(), args.to_vec())
        .ok()
        .and_then(|subst| substitute_complete(db, signature, &subst).ok())
    else {
        return EvidenceList::none(db);
    };
    EvidenceList::of(
        db,
        goals
            .list(db)
            .iter()
            .enumerate()
            .map(|(index, &goal)| closed(index).or_else(|| prove(db, solve_cx, goal)))
            .collect(),
    )
}

fn prove_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _solve_cx: TraitSolveCx<'db>,
    _goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    None
}

fn prove_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Option<EvidenceId<'db>>,
    _count: u32,
    _solve_cx: TraitSolveCx<'db>,
    _goal: TraitInstId<'db>,
) -> salsa::CycleRecoveryAction<Option<EvidenceId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}
