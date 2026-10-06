//! Access checking of normalized semantic bodies.
//!
//! Mutable value semantics: references exist only as parameter modes, named
//! access bindings and projection results. The checker tracks the accesses a
//! body opens (borrows, views and projection sessions) between their opening
//! and the `end` that liveness elaboration placed after their last use, and
//! enforces one rule: overlapping accesses conflict unless both read. Calls
//! are checked against signatures only: a call's interference footprint is
//! its data arguments plus the footprints of its declared effects. See
//! `docs/architecture/access-checker.md`.
mod check;
mod control;
mod domain;
mod place;
mod refine;

use std::{collections::VecDeque, fmt};

use common::diagnostics::CompleteDiagnostic;
use rustc_hash::FxHashSet;
use salsa::Update;

pub use self::control::{
    ExecutableBlock, ExecutableControlFlow, semantic_executable_control_flow, semantic_may_return,
};
pub(crate) use self::refine::{CallSiteRefinements, provisional_call_site_provider_refinements};
use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SemanticInstance,
            diagnostics::{
                BlockedSemanticBody, SemanticDiagnosticId, SemanticNormalizationFailure,
            },
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalized::normalize_semantic_body,
        },
        ty::{provider::ProviderAddressSpace, ty_check::BodyOwner},
    },
    hir_def::{ItemKind, TopLevelMod},
};

#[derive(Clone, Debug, PartialEq, Eq, Update)]
pub enum SemanticAccessCheckResult<'db> {
    /// Checked, with the body's warnings and, for a projection, the address
    /// space each access component yields: its result-space contract.
    Ok {
        warnings: Vec<SemanticDiagnosticId<'db>>,
        spaces: Vec<Option<ProviderAddressSpace>>,
    },
    Blocked(BlockedSemanticBody<'db>),
    Err(SemanticDiagnosticId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticAnalysisError<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Diagnostic(CompleteDiagnostic),
}

impl SemanticAnalysisError<'_> {
    pub fn diagnostic(&self) -> Option<&CompleteDiagnostic> {
        match self {
            Self::Blocked(_) => None,
            Self::Diagnostic(diag) => Some(diag),
        }
    }
}

impl fmt::Display for SemanticAnalysisError<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blocked(blocked) => write!(
                formatter,
                "semantic body is blocked by upstream causes: {:?}",
                blocked.causes
            ),
            Self::Diagnostic(diag) => formatter.write_str(&diag.message),
        }
    }
}

/// A projection's result-space contract, per access component, as its body
/// infers it: callers depend on this exported fact, never on the body.
pub(crate) fn projection_result_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> &'db [Option<ProviderAddressSpace>] {
    match body_check(db, instance) {
        SemanticAccessCheckResult::Ok { spaces, .. } => spaces,
        SemanticAccessCheckResult::Blocked(_) | SemanticAccessCheckResult::Err(_) => &[],
    }
}

// A projection's check reads its projection callees' contracts, so recursion
// through projections, which the check rejects, is a cycle.
#[salsa::tracked(
    return_ref,
    cycle_fn = body_check_cycle_recover,
    cycle_initial = body_check_cycle_initial
)]
fn body_check<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticAccessCheckResult<'db> {
    let artifacts = match normalize_semantic_body(db, instance) {
        Ok(artifacts) => artifacts,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return SemanticAccessCheckResult::Blocked(blocked);
        }
        Err(
            SemanticNormalizationFailure::Rejected(diag)
            | SemanticNormalizationFailure::InternalFailure(diag),
        ) => return SemanticAccessCheckResult::Err(SemanticDiagnosticId::new(db, diag)),
    };
    match check::check_body(db, instance, &artifacts.body) {
        Ok((warnings, spaces)) => SemanticAccessCheckResult::Ok {
            warnings: warnings
                .into_iter()
                .map(|diag| SemanticDiagnosticId::new(db, diag))
                .collect(),
            spaces,
        },
        Err(diag) => SemanticAccessCheckResult::Err(SemanticDiagnosticId::new(db, diag)),
    }
}

fn body_check_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: SemanticInstance<'db>,
) -> SemanticAccessCheckResult<'db> {
    SemanticAccessCheckResult::Ok {
        warnings: Vec::new(),
        spaces: Vec::new(),
    }
}

fn body_check_cycle_recover<'db>(
    _: &'db dyn HirAnalysisDb,
    _: &SemanticAccessCheckResult<'db>,
    _: u32,
    _: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<SemanticAccessCheckResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

pub fn check_semantic_accesses<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticAnalysisError<'db>> {
    match body_check(db, instance) {
        SemanticAccessCheckResult::Ok { .. } => Ok(()),
        SemanticAccessCheckResult::Blocked(body) => {
            Err(SemanticAnalysisError::Blocked(body.clone()))
        }
        SemanticAccessCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

pub struct SemanticAccessAnalysisPass;

impl ModuleAnalysisPass for SemanticAccessAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        collect_semantic_access_diagnostic_vouchers(db, top_mod)
    }
}

/// Checks each item's identity instance and, transitively, the concrete
/// instances it calls.
pub fn collect_semantic_access_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    let mut owners = Vec::new();
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        owners.extend(BodyOwner::const_predicates_of(db, *item));
        match item {
            ItemKind::Func(func) => owners.push(BodyOwner::Func(*func)),
            ItemKind::Const(const_) => owners.push(BodyOwner::Const(*const_)),
            ItemKind::Contract(contract) => {
                owners.push(BodyOwner::ContractInit {
                    contract: *contract,
                });
                for (recv_idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(db).len() {
                        owners.push(BodyOwner::ContractRecvArm {
                            contract: *contract,
                            recv_idx: recv_idx as u32,
                            arm_idx: arm_idx as u32,
                        });
                    }
                }
            }
            ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }
    let mut pending: VecDeque<_> = owners
        .into_iter()
        .map(|owner| get_or_build_semantic_instance(db, identity_semantic_instance_key(db, owner)))
        .collect();
    // Warnings are reported once, for the module's own bodies.
    let own: FxHashSet<_> = pending.iter().copied().collect();
    let mut seen = FxHashSet::default();
    let mut seen_diags = FxHashSet::default();
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = Vec::new();
    while let Some(instance) = pending.pop_front() {
        if !seen.insert(instance) {
            continue;
        }
        match body_check(db, instance) {
            SemanticAccessCheckResult::Ok { warnings, .. } => {
                if own.contains(&instance) {
                    diags.extend(
                        warnings
                            .iter()
                            .map(|diag| Box::new(*diag) as Box<dyn DiagnosticVoucher + 'db>),
                    );
                }
                pending.extend(
                    instance
                        .callees(db)
                        .iter()
                        .map(|callee| get_or_build_semantic_instance(db, callee.key)),
                );
            }
            SemanticAccessCheckResult::Blocked(_) => {}
            SemanticAccessCheckResult::Err(diag) => {
                if seen_diags.insert(*diag) {
                    diags.push(Box::new(*diag));
                }
            }
        }
    }
    diags
}
