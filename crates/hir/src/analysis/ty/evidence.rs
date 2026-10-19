//! Evidence: how a trait goal holds.
//!
//! The checker proves each trait goal a body uses once, in the body's
//! context, and records the proof. A proof is an implementation with a proof
//! of each of its conditions, or one of the body's assumptions, named by its
//! position in the body's assumption list.

use salsa::Update;

use super::{
    fold::{TyFoldable, TyFolder},
    normalize::normalize_ty,
    trait_def::{
        ImplementorOrigin, ResolvedImplInstance, TraitInstId, resolve_trait_impl_instance,
    },
    trait_resolution::{Selection, TraitSolveCx, answering_assumption},
    ty_def::{BorrowKind, TyFlags},
    visitor::{TyVisitable, TyVisitor, collect_flags},
};
use crate::analysis::HirAnalysisDb;

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
#[salsa::tracked(cycle_fn = prove_cycle_recover, cycle_initial = prove_cycle_initial)]
pub fn prove<'db>(
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
                resolved,
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
            resolved,
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
        // An instantiated body can expose projections its checker could not
        // resolve.
        Some(resolved) if collect_flags(db, resolved).contains(TyFlags::HAS_PROJECTION) => {
            Some(normalize_ty(
                db,
                resolved,
                solve_cx.normalization_scope_for_trait_inst(db, inst),
                solve_cx.assumptions(),
            ))
        }
        Some(resolved) => Some(resolved),
        None => match resolve_trait_impl_instance(db, solve_cx, inst) {
            Selection::Unique(resolved) => Some(resolved),
            Selection::Ambiguous(_) | Selection::NotFound => None,
        },
    }
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
