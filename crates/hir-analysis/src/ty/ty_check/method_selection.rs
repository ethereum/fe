use either::Either;
use hir::{
    hir_def::{scope_graph::ScopeId, IdentId, Trait},
    span::DynLazySpan,
};
use rustc_hash::FxHashSet;

use crate::{
    name_resolution::{available_traits_in_scope, diagnostics::NameResDiag, is_scope_visible_from},
    ty::{
        canonical::{Canonical, Solution},
        diagnostics::{BodyDiag, FuncBodyDiag},
        func_def::FuncDef,
        method_table::probe_method,
        trait_def::{impls_of_ty, Implementor, TraitDef, TraitInstId, TraitMethod},
        trait_lower::lower_trait,
        trait_resolution::PredicateListId,
        ty_def::TyId,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::From)]
pub(super) enum Candidate {
    InherentMethod(FuncDef),
    TraitMethod(TraitMethodCand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct TraitMethodCand {
    pub(super) inst: Solution<TraitInstId>,
    pub(super) method: TraitMethod,
    implementor: Option<Implementor>,
}

impl TraitMethodCand {
    fn new(
        inst: Solution<TraitInstId>,
        method: TraitMethod,
        implementor: Option<Implementor>,
    ) -> Self {
        Self {
            inst,
            method,
            implementor,
        }
    }
}

pub(super) fn select_method_candidate(
    db: &dyn HirAnalysisDb,
    receiver: (Canonical<TyId>, DynLazySpan),
    method_name: (IdentId, DynLazySpan),
    scope: ScopeId,
    assumptions: PredicateListId,
) -> Result<Candidate, FuncBodyDiag> {
    if receiver.0.value.is_ty_var(db) {
        return Err(BodyDiag::TypeMustBeKnown(method_name.1).into());
    }

    let candidates = assemble_method_candidates(db, receiver.0, method_name.0, scope, assumptions);

    let selector = MethodSelector {
        db,
        scope,
        candidates: &candidates,
        assumptions,
    };

    match selector.select() {
        Ok(candidate) => Ok(candidate),

        Err(MethodSelectionError::AmbiguousInherentMethod(cands)) => {
            let cand_spans = cands.into_iter().map(|cand| cand.name_span(db)).collect();
            let diag = BodyDiag::AmbiguousInherentMethodCall {
                primary: method_name.1,
                method_name: method_name.0,
                cand_spans,
            };

            Err(diag.into())
        }

        Err(MethodSelectionError::AmbiguousTraitMethod(cands)) => {
            let cands = cands
                .into_iter()
                .map(|cand| format!("`{}`", cand.pretty_print(db, true)))
                .collect();

            let diag = BodyDiag::AmbiguousTraitMethodCall {
                primary: method_name.1,
                method_name: method_name.0,
                cand_insts: cands,
            };

            Err(diag.into())
        }

        Err(MethodSelectionError::NotFound) => {
            let base_ty = receiver.0.value.base_ty(db);
            let diag =
                BodyDiag::method_not_found(db, method_name.1, method_name.0, Either::Left(base_ty));
            Err(diag.into())
        }

        Err(MethodSelectionError::InvisibleInherentMethod(func)) => {
            let diag =
                NameResDiag::invisible(method_name.1, method_name.0, func.name_span(db).into());
            Err(diag.into())
        }

        Err(MethodSelectionError::InvisibleTraitMethod(traits)) => {
            let diag = BodyDiag::InvisibleTraitMethod {
                primary: method_name.1,
                traits,
            };
            Err(diag.into())
        }
    }
}

fn assemble_method_candidates(
    db: &dyn HirAnalysisDb,
    receiver_ty: Canonical<TyId>,
    method_name: IdentId,
    scope: ScopeId,
    assumptions: PredicateListId,
) -> Vec<Candidate> {
    CandidateAssembler {
        db,
        receiver_ty,
        method_name,
        scope,
        assumptions,
        candidates: Vec::new(),
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
    assumptions: PredicateListId,
    candidates: Vec<Candidate>,
}

impl<'db> CandidateAssembler<'db> {
    fn assemble(mut self) -> Vec<Candidate> {
        self.assemble_method_candidates();
        self.assemble_trait_method_candidates();
        self.candidates
    }

    fn assemble_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db.as_hir_db());
        for &method in probe_method(self.db, ingot, self.receiver_ty, self.method_name) {
            self.candidates.push(Candidate::InherentMethod(method));
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db.as_hir_db());

        for &implementor in impls_of_ty(self.db, ingot, self.receiver_ty) {
            let mut table = UnificationTable::new(self.db);
            let implementor = table.instantiate_with_fresh_vars(implementor);

            if let Some(trait_method) = implementor
                .trait_def(self.db)
                .methods(self.db)
                .get(&self.method_name)
            {
                let solution = self
                    .receiver_ty
                    .make_solution(self.db, implementor.trait_(self.db));
                let cand = TraitMethodCand::new(solution, *trait_method, implementor.into());

                self.candidates.push(cand.into());
            }
        }

        let mut table = UnificationTable::new(self.db);
        let receiver_ty = self.receiver_ty.extract_identity(&mut table);

        let mut insert_trait_method_cand = |inst: TraitInstId| {
            if let Some(trait_method) = inst.def(self.db).methods(self.db).get(&self.method_name) {
                let solution = self.receiver_ty.make_solution(self.db, inst);
                let cand = TraitMethodCand::new(solution, *trait_method, None);
                self.candidates.push(cand.into());
            }
        };
        for &pred in self.assumptions.list(self.db) {
            let snapshot = table.snapshot();
            let self_ty = pred.self_ty(self.db);
            let self_ty = table.instantiate_to_term(self_ty);

            if table.unify(receiver_ty, self_ty).is_ok() {
                insert_trait_method_cand(pred);
                for super_trait in pred.def(self.db).super_traits(self.db) {
                    insert_trait_method_cand(super_trait.instantiate(self.db, pred.args(self.db)))
                }
            }

            table.rollback_to(snapshot);
        }
    }
}

struct MethodSelector<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId,
    candidates: &'db [Candidate],
    assumptions: PredicateListId,
}

impl<'db> MethodSelector<'db> {
    fn select(self) -> Result<Candidate, MethodSelectionError> {
        let inherent_cands: Vec<_> = self
            .candidates
            .iter()
            .filter_map(|&candidate| match candidate {
                Candidate::InherentMethod(func_def) => Some(func_def),
                _ => None,
            })
            .collect();
        let visible_inherent_cands: Vec<_> = inherent_cands
            .iter()
            .copied()
            .filter(|cand| self.is_inherent_method_visible(*cand))
            .collect();

        match visible_inherent_cands.len() {
            0 => {
                if !inherent_cands.is_empty() {
                    return Err(MethodSelectionError::InvisibleInherentMethod(
                        inherent_cands[0],
                    ));
                }
            }
            1 => {
                return Ok(Candidate::InherentMethod(visible_inherent_cands[0]));
            }

            _ => {
                return Err(MethodSelectionError::AmbiguousInherentMethod(
                    inherent_cands,
                ))
            }
        };

        let available_traits = self.available_traits();
        let trait_cands: Vec<_> = self
            .candidates
            .iter()
            .filter_map(|&candidate| match candidate {
                Candidate::TraitMethod(cand) => Some(cand),

                _ => None,
            })
            .collect();
        let visible_trait_cands: Vec<_> = trait_cands
            .iter()
            .copied()
            .filter(|cand| available_traits.contains(&cand.inst.value.def(self.db)))
            .collect();

        match visible_trait_cands.len() {
            0 => {}
            1 => return Ok(Candidate::TraitMethod(visible_trait_cands[0])),
            _ => {
                return Err(MethodSelectionError::AmbiguousTraitMethod(
                    visible_trait_cands
                        .into_iter()
                        .map(|cand| {
                            if let Some(implementor) = cand.implementor {
                                implementor.trait_(self.db)
                            } else {
                                cand.inst.value
                            }
                        })
                        .collect(),
                ));
            }
        }

        if !trait_cands.is_empty() {
            // Suggests trait imports.
            let traits = trait_cands
                .into_iter()
                .map(|cand| cand.inst.value.def(self.db).trait_(self.db))
                .collect();
            Err(MethodSelectionError::InvisibleTraitMethod(traits))
        } else {
            Err(MethodSelectionError::NotFound)
        }
    }

    fn is_inherent_method_visible(&self, def: FuncDef) -> bool {
        is_scope_visible_from(self.db, def.scope(self.db), self.scope)
    }

    fn available_traits(&self) -> FxHashSet<TraitDef> {
        let mut traits = FxHashSet::default();

        let mut insert_trait = |trait_def: TraitDef| {
            traits.insert(trait_def);

            for trait_ in trait_def.super_traits(self.db) {
                traits.insert(trait_.skip_binder().def(self.db));
            }
        };

        for &trait_ in available_traits_in_scope(self.db, self.scope) {
            let trait_def = lower_trait(self.db, trait_);
            insert_trait(trait_def);
        }

        for pred in self.assumptions.list(self.db) {
            let trait_def = pred.def(self.db);
            insert_trait(trait_def)
        }

        traits
    }
}

pub enum MethodSelectionError {
    AmbiguousInherentMethod(Vec<FuncDef>),
    AmbiguousTraitMethod(Vec<TraitInstId>),
    NotFound,
    InvisibleInherentMethod(FuncDef),
    InvisibleTraitMethod(Vec<Trait>),
}
