use either::Either;
use hir::{
    hir_def::{scope_graph::ScopeId, IdentId, Trait},
    span::DynLazySpan,
};
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::{
    name_resolution::{available_traits_in_scope, diagnostics::NameResDiag, is_scope_visible_from},
    ty::{
        canonical::{Canonical, Canonicalized, Solution},
        diagnostics::{BodyDiag, FuncBodyDiag},
        fold::TyFoldable,
        func_def::FuncDef,
        method_table::probe_method,
        trait_def::{impls_for_ty, TraitDef, TraitInstId, TraitMethod},
        trait_lower::lower_trait,
        trait_resolution::{is_goal_satisfiable, GoalSatisfiability, PredicateListId},
        ty_def::TyId,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Candidate {
    InherentMethod(FuncDef),
    TraitMethod(TraitMethodCand),
    NeedsConfirmation(TraitMethodCand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct TraitMethodCand {
    pub(super) inst: Solution<TraitInstId>,
    pub(super) method: TraitMethod,
}

impl TraitMethodCand {
    fn new(inst: Solution<TraitInstId>, method: TraitMethod) -> Self {
        Self { inst, method }
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
        receiver: receiver.0,
        scope,
        candidates,
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

        Err(MethodSelectionError::AmbiguousTraitMethod(traits)) => {
            let traits = traits.into_iter().map(|def| def.trait_(db)).collect();

            let diag = BodyDiag::AmbiguousTrait {
                primary: method_name.1,
                method_name: method_name.0,
                traits,
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
) -> AssembledCandidates {
    CandidateAssembler {
        db,
        receiver_ty,
        method_name,
        scope,
        assumptions,
        candidates: AssembledCandidates::default(),
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
    candidates: AssembledCandidates,
}

impl<'db> CandidateAssembler<'db> {
    fn assemble(mut self) -> AssembledCandidates {
        self.assemble_inherent_method_candidates();
        self.assemble_trait_method_candidates();
        self.candidates
    }

    fn assemble_inherent_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db.as_hir_db());
        for &method in probe_method(self.db, ingot, self.receiver_ty, self.method_name) {
            self.candidates.insert_inherent_method(method);
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db.as_hir_db());
        let mut table = UnificationTable::new(self.db);
        let extracted_receiver_ty = self.receiver_ty.extract_identity(&mut table);

        for &implementor in impls_for_ty(self.db, ingot, self.receiver_ty) {
            let trait_def = implementor.skip_binder().trait_def(self.db);
            self.insert_trait_method_cand(trait_def)
        }

        for &pred in self.assumptions.list(self.db) {
            let snapshot = table.snapshot();
            let self_ty = pred.self_ty(self.db);
            let self_ty = table.instantiate_to_term(self_ty);

            if table.unify(extracted_receiver_ty, self_ty).is_ok() {
                self.insert_trait_method_cand(pred.def(self.db));
                for super_trait in pred.def(self.db).super_traits(self.db) {
                    let super_trait = super_trait.instantiate(self.db, pred.args(self.db));
                    self.insert_trait_method_cand(super_trait.def(self.db));
                }
            }

            table.rollback_to(snapshot);
        }
    }

    fn insert_trait_method_cand(&mut self, trait_def: TraitDef) {
        if let Some(&trait_method) = trait_def.methods(self.db).get(&self.method_name) {
            self.candidates.insert_trait(trait_def, trait_method);
        }
    }
}

struct MethodSelector<'db> {
    db: &'db dyn HirAnalysisDb,
    receiver: Canonical<TyId>,
    scope: ScopeId,
    candidates: AssembledCandidates,
    assumptions: PredicateListId,
}

impl<'db> MethodSelector<'db> {
    fn select(self) -> Result<Candidate, MethodSelectionError> {
        if let Some(res) = self.select_inherent_method() {
            return res;
        }

        self.select_trait_methods()
    }

    fn select_inherent_method(&self) -> Option<Result<Candidate, MethodSelectionError>> {
        let inherent_methods = &self.candidates.inherent_methods;
        let visible_inherent_methods: Vec<_> = inherent_methods
            .iter()
            .copied()
            .filter(|cand| self.is_inherent_method_visible(*cand))
            .collect();

        match visible_inherent_methods.len() {
            0 => {
                if inherent_methods.is_empty() {
                    None
                } else {
                    Some(Err(MethodSelectionError::InvisibleInherentMethod(
                        *inherent_methods.iter().next().unwrap(),
                    )))
                }
            }
            1 => Some(Ok(Candidate::InherentMethod(visible_inherent_methods[0]))),

            _ => Some(Err(MethodSelectionError::AmbiguousInherentMethod(
                inherent_methods.iter().copied().collect(),
            ))),
        }
    }

    fn select_trait_methods(&self) -> Result<Candidate, MethodSelectionError> {
        let available_traits = self.available_traits();
        let traits = &self.candidates.traits;
        let visible_traits: Vec<_> = traits
            .iter()
            .copied()
            .filter(|cand| available_traits.contains(&cand.0))
            .collect();

        match visible_traits.len() {
            0 => {
                if traits.is_empty() {
                    Err(MethodSelectionError::NotFound)
                } else {
                    // Suggests trait imports.
                    let traits = traits.iter().map(|(def, _)| def.trait_(self.db)).collect();
                    Err(MethodSelectionError::InvisibleTraitMethod(traits))
                }
            }

            1 => {
                let (def, method) = visible_traits[0];
                Ok(self.find_inst(def, method))
            }

            _ => Err(MethodSelectionError::AmbiguousTraitMethod(
                visible_traits.into_iter().map(|cand| cand.0).collect(),
            )),
        }
    }

    fn find_inst(&self, def: TraitDef, method: TraitMethod) -> Candidate {
        let mut table = UnificationTable::new(self.db);
        let receiver = self.receiver.extract_identity(&mut table);

        // Assign type variables to trait parameters.
        let inst_args = def
            .params(self.db)
            .iter()
            .map(|ty| table.new_var_from_param(*ty))
            .collect_vec();

        let cand = TraitInstId::new(self.db, def, inst_args);
        // Unify receiver and method self.
        method.instantiate_with_inst(&mut table, receiver, cand);

        let cand = cand.fold_with(&mut table);
        let canonical_cand = Canonicalized::new(self.db, cand);

        match is_goal_satisfiable(self.db, self.assumptions, canonical_cand.value) {
            GoalSatisfiability::Satisfied(solution) => {
                // Map back the solution to the current context.
                let solution = canonical_cand.extract_solution(&mut table, *solution);

                // Unify candidate to solution.
                table.unify(cand, solution).unwrap();

                Candidate::TraitMethod(TraitMethodCand::new(
                    self.receiver.make_solution(self.db, &mut table, cand),
                    method,
                ))
            }

            &GoalSatisfiability::NeedsConfirmation(_)
            | GoalSatisfiability::ContainsInvalid
            | GoalSatisfiability::UnSat => Candidate::NeedsConfirmation(TraitMethodCand::new(
                self.receiver.make_solution(self.db, &mut table, cand),
                method,
            )),
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
    AmbiguousTraitMethod(Vec<TraitDef>),
    NotFound,
    InvisibleInherentMethod(FuncDef),
    InvisibleTraitMethod(Vec<Trait>),
}

#[derive(Default)]
struct AssembledCandidates {
    inherent_methods: FxHashSet<FuncDef>,
    traits: FxHashSet<(TraitDef, TraitMethod)>,
}

impl AssembledCandidates {
    fn insert_inherent_method(&mut self, method: FuncDef) {
        self.inherent_methods.insert(method);
    }

    fn insert_trait(&mut self, def: TraitDef, method: TraitMethod) {
        self.traits.insert((def, method));
    }
}
