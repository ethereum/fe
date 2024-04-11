use hir::{
    hir_def::{scope_graph::ScopeId, IdentId},
    span::DynLazySpan,
};

use crate::{
    name_resolution::{diagnostics::NameResDiag, is_scope_visible_from},
    ty::{
        canonical::Canonical,
        constraint::AssumptionListId,
        diagnostics::{BodyDiag, FuncBodyDiag},
        method_table::collect_methods,
        trait_def::{Implementor, TraitDef},
        ty_def::{FuncDef, TyId},
    },
    HirAnalysisDb,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Candidate {
    InherentMethod(FuncDef),
    TraitMethod(FuncDef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssembleMode {
    InScope,
    Global,
}

pub(super) fn select_method_candidate(
    db: &dyn HirAnalysisDb,
    receiver: (Canonical<TyId>, DynLazySpan),
    method_name: (IdentId, DynLazySpan),
    scope: ScopeId,
    assumptions: AssumptionListId,
) -> Result<Candidate, FuncBodyDiag> {
    if receiver.0.value.is_ty_var(db) {
        return Err(BodyDiag::TypeMustBeKnown(receiver.1).into());
    }

    let candidates = assemble_method_candidates(
        db,
        receiver.0,
        method_name.0,
        scope,
        assumptions,
        AssembleMode::InScope,
    );

    let mut selector = MethodSelector {
        db,
        scope,
        candidates: &candidates,
    };

    match selector.select() {
        Ok(candidate) => Ok(candidate),

        Err(MethodSelectionError::Ambiguous(cands)) => {
            let cands = cands.into_iter().map(|cand| cand.hir_func(db)).collect();
            let diag = BodyDiag::AmbiguousMethodCall {
                primary: method_name.1,
                method_name: method_name.0,
                cands,
            };

            Err(diag.into())
        }

        Err(MethodSelectionError::NotFound) => {
            let base_ty = receiver.0.value.base_ty(db);
            let diag = BodyDiag::method_not_found(db, method_name.1, method_name.0, base_ty);
            Err(diag.into())
        }

        Err(MethodSelectionError::Invisible(func)) => {
            let diag =
                NameResDiag::invisible(method_name.1, method_name.0, func.name_span(db).into());
            Err(diag.into())
        }
    }
}

fn assemble_method_candidates(
    db: &dyn HirAnalysisDb,
    receiver_ty: Canonical<TyId>,
    method_name: IdentId,
    scope: ScopeId,
    assumptions: AssumptionListId,
    mode: AssembleMode,
) -> Vec<Candidate> {
    CandidateAssembler {
        db,
        receiver_ty,
        method_name,
        scope,
        assumptions,
        candidates: Vec::new(),
        mode: AssembleMode::InScope,
    }
    .assemble()
}

struct CandidateAssembler<'db> {
    db: &'db dyn HirAnalysisDb,
    /// The type that method is being called on.
    receiver_ty: Canonical<TyId>,
    /// The name of the method being called.
    method_name: IdentId,
    /// The scope that candidates are being assembled in.
    scope: ScopeId,
    /// The assumptions for the type bound in the current scope.
    assumptions: AssumptionListId,
    candidates: Vec<Candidate>,

    mode: AssembleMode,
}

impl<'db> CandidateAssembler<'db> {
    fn assemble(mut self) -> Vec<Candidate> {
        self.assemble_method_candidates();
        // self.assemble_trait_method_candidates();
        self.candidates
    }

    fn assemble_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db.as_hir_db());
        let method_table = collect_methods(self.db, ingot);
        if let Some(func_def) = method_table.probe(self.db, self.receiver_ty, self.method_name) {
            self.candidates.push(Candidate::InherentMethod(func_def));
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        todo!()
    }
}

struct MethodSelector<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId,
    candidates: &'db [Candidate],
}

impl<'db> MethodSelector<'db> {
    fn select(self) -> Result<Candidate, MethodSelectionError> {
        let inherent_methods: Vec<_> = self
            .candidates
            .iter()
            .filter_map(|&candidate| match candidate {
                Candidate::InherentMethod(func_def) => Some(func_def),
                _ => None,
            })
            .collect();

        match inherent_methods.len() {
            0 => {}
            1 => {
                let method = inherent_methods[0];
                if self.is_visible(method) {
                    return Ok(Candidate::InherentMethod(method));
                }
            }
            _ => return Err(MethodSelectionError::Ambiguous(inherent_methods)),
        };

        // TODO: Fallback to trait method selection.

        if !inherent_methods.is_empty() {
            return Err(MethodSelectionError::Invisible(inherent_methods[0]));
        }

        Err(MethodSelectionError::NotFound)
    }

    fn is_visible(&self, def: FuncDef) -> bool {
        is_scope_visible_from(self.db, def.hir_func(self.db).scope(), self.scope)
    }
}

pub enum MethodSelectionError {
    Ambiguous(Vec<FuncDef>),
    NotFound,
    Invisible(FuncDef),
}
