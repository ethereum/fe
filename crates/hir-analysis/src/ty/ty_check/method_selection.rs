use hir::{
    hir_def::{scope_graph::ScopeId, IdentId, Trait},
    span::DynLazySpan,
};
use rustc_hash::FxHashSet;

use crate::{
    name_resolution::{available_traits_in_scope, diagnostics::NameResDiag, is_scope_visible_from},
    ty::{
        canonical::Canonical,
        constraint::{collect_super_traits, AssumptionListId},
        diagnostics::{BodyDiag, FuncBodyDiag},
        method_table::collect_methods,
        trait_def::{impls_of_ty, Implementor, TraitDef, TraitInstId, TraitMethod},
        trait_lower::lower_trait,
        ty_def::{FuncDef, TyId},
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
    pub(super) inst: Canonical<TraitInstId>,
    pub(super) method: TraitMethod,
    implementor: Option<Implementor>,
}

impl TraitMethodCand {
    fn new(
        db: &dyn HirAnalysisDb,
        inst: TraitInstId,
        method: TraitMethod,
        implementor: Option<Implementor>,
    ) -> Self {
        let inst = Canonical::canonicalize(db, inst);
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
    assumptions: AssumptionListId,
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
                .map(|cand| {
                    format!(
                        "`{}`: `{}`",
                        cand.self_ty(db).pretty_print(db),
                        cand.pretty_print(db)
                    )
                })
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
            let diag = BodyDiag::method_not_found(db, method_name.1, method_name.0, base_ty);
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
    assumptions: AssumptionListId,
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
    assumptions: AssumptionListId,
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
        let method_table = collect_methods(self.db, ingot);
        for method in method_table.probe(self.db, self.receiver_ty, self.method_name) {
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
                let cand = TraitMethodCand::new(
                    self.db,
                    implementor.trait_(self.db),
                    *trait_method,
                    implementor.into(),
                );

                self.candidates.push(cand.into());
            }
        }

        let mut table = UnificationTable::new(self.db);
        let receiver_ty = self.receiver_ty.decanonicalize(&mut table);
        for &pred in self.assumptions.predicates(self.db) {
            let snapshot = table.snapshot();

            if table.unify(receiver_ty, pred.ty(self.db)).is_ok() {
                if let Some(trait_method) = pred
                    .trait_inst(self.db)
                    .def(self.db)
                    .methods(self.db)
                    .get(&self.method_name)
                {
                    let cand = TraitMethodCand::new(
                        self.db,
                        pred.trait_inst(self.db),
                        *trait_method,
                        None,
                    );
                    self.candidates.push(cand.into());
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
    assumptions: AssumptionListId,
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
            0 => {}
            1 => {
                return Ok(Candidate::InherentMethod(visible_inherent_cands[0]));
            }

            _ => {
                return Err(MethodSelectionError::AmbiguousInherentMethod(
                    inherent_cands,
                ))
            }
        };

        if !inherent_cands.is_empty() {
            return Err(MethodSelectionError::InvisibleInherentMethod(
                inherent_cands[0],
            ));
        }

        let available_traits = self.available_traits();

        let trait_cands: Vec<_> = self
            .candidates
            .iter()
            .filter_map(|&candidate| match candidate {
                Candidate::TraitMethod(cand) => Some(cand),

                _ => None,
            })
            .collect();

        let available_trait_cands: Vec<_> = trait_cands
            .iter()
            .copied()
            .filter(|cand| available_traits.contains(&cand.inst.value.def(self.db)))
            .collect();

        match available_trait_cands.len() {
            0 => {}
            1 => return Ok(Candidate::TraitMethod(available_trait_cands[0])),
            _ => {
                return Err(MethodSelectionError::AmbiguousTraitMethod(
                    available_trait_cands
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

        // Suggests trait imports.
        if !trait_cands.is_empty() {
            let traits = trait_cands
                .into_iter()
                .map(|cand| cand.inst.value.def(self.db).trait_(self.db))
                .collect();
            return Err(MethodSelectionError::InvisibleTraitMethod(traits));
        }

        Err(MethodSelectionError::NotFound)
    }

    fn is_inherent_method_visible(&self, def: FuncDef) -> bool {
        is_scope_visible_from(self.db, def.scope(self.db), self.scope)
    }

    fn available_traits(&self) -> FxHashSet<TraitDef> {
        let mut traits = FxHashSet::default();

        let mut insert_trait = |trait_def: TraitDef| {
            traits.insert(trait_def);
            let Ok(super_traits) = collect_super_traits(self.db, trait_def) else {
                return;
            };

            for trait_ in super_traits.skip_binder() {
                traits.insert(trait_.def(self.db));
            }
        };

        for &trait_ in available_traits_in_scope(self.db, self.scope) {
            let trait_def = lower_trait(self.db, trait_);
            insert_trait(trait_def);
        }

        for pred in self.assumptions.predicates(self.db) {
            let trait_def = pred.trait_inst(self.db).def(self.db);
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
