use salsa::Update;

use super::ty_def::TyId;
use super::{
    fold::{TyFoldable, TyFolder},
    trait_choice::{Choice, DeliveryList, Resolved, SlotChoices, SlotRef},
    trait_def::TraitInstId,
    trait_resolution::{PredicateListId, TraitSolveCx},
    visitor::{TyVisitable, TyVisitor},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{IdentId, Impl, scope_graph::ScopeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct AssocConstUse<'db> {
    origin_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
    /// The checker's decision for `inst`'s slot in value position. Always
    /// `None` in a type: a type that depends on a choice is refused.
    choice: Option<Choice<'db>>,
    /// The decisions for the bounds of the constant body that runs.
    delivered: Option<DeliveryList<'db>>,
    /// The checker's proof of the constant use's trait goal.
    evidence: Option<super::evidence::EvidenceId<'db>>,
    /// Proofs of the assumptions the constant's value body is checked under.
    bound_evidence: Option<super::evidence::EvidenceList<'db>>,
}

impl<'db> AssocConstUse<'db> {
    pub fn new(
        origin_scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        inst: TraitInstId<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self {
            origin_scope,
            assumptions,
            inst,
            name,
            choice: None,
            delivered: None,
            evidence: None,
            bound_evidence: None,
        }
    }

    pub fn choice(self) -> Option<Choice<'db>> {
        self.choice
    }

    pub fn delivered(self) -> Option<DeliveryList<'db>> {
        self.delivered
    }

    pub fn evidence(self) -> Option<super::evidence::EvidenceId<'db>> {
        self.evidence
    }

    pub fn bound_evidence(self) -> Option<super::evidence::EvidenceList<'db>> {
        self.bound_evidence
    }

    pub fn with_delivered(self, delivered: Option<DeliveryList<'db>>) -> Self {
        Self { delivered, ..self }
    }

    pub fn with_choice(self, choice: Option<Choice<'db>>) -> Self {
        Self { choice, ..self }
    }

    pub fn with_evidence(self, evidence: Option<super::evidence::EvidenceId<'db>>) -> Self {
        Self { evidence, ..self }
    }

    pub fn with_bound_evidence(
        self,
        bound_evidence: Option<super::evidence::EvidenceList<'db>>,
    ) -> Self {
        Self {
            bound_evidence,
            ..self
        }
    }

    pub(crate) fn bind_evidence(
        self,
        db: &'db dyn HirAnalysisDb,
        bound: super::evidence::EvidenceList<'db>,
    ) -> Self {
        let evidence = self.evidence.map(|ev| ev.bind(db, bound));
        let bound_evidence = self.bound_evidence.map(|ev| ev.bind(db, bound));
        let choice = match evidence {
            Some(ev) if ev.is_pending(db) => Some(Choice::Slot(SlotRef::Unsupplied)),
            Some(ev) => super::trait_choice::KnownChoice::from_evidence(db, ev, self.inst)
                .map(Choice::Known)
                .or(self.choice),
            None => self.choice,
        };
        Self {
            choice,
            evidence,
            bound_evidence,
            ..self
        }
    }

    pub fn origin_scope(self) -> ScopeId<'db> {
        self.origin_scope
    }

    pub fn assumptions(self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn inst(self) -> TraitInstId<'db> {
        self.inst
    }

    pub fn with_inst(self, inst: TraitInstId<'db>) -> Self {
        Self { inst, ..self }
    }

    pub fn with_env(self, origin_scope: ScopeId<'db>, assumptions: PredicateListId<'db>) -> Self {
        Self {
            origin_scope,
            assumptions,
            ..self
        }
    }

    pub fn name(self) -> IdentId<'db> {
        self.name
    }

    pub fn solve_cx(self, db: &'db dyn HirAnalysisDb) -> TraitSolveCx<'db> {
        TraitSolveCx::new(db, self.origin_scope).with_assumptions(self.assumptions)
    }

    /// This use read inside an instance that carries `choices`: its own
    /// decision and the decisions it delivers to the constant body.
    pub fn specialize(self, db: &'db dyn HirAnalysisDb, choices: &SlotChoices<'db>) -> Self {
        let delivered = self.delivered.map(|list| list.specialize(db, choices));
        let this = Self { delivered, ..self };
        if this.choice.is_none() {
            return this;
        }
        match choices.resolve_in(db, this.choice) {
            Resolved::Known(known) => this.with_choice(Some(Choice::Known(known))),
            Resolved::Pending => this.with_choice(Some(Choice::Slot(SlotRef::Unsupplied))),
            Resolved::Ordinary => this,
        }
    }

    /// This use without ordinary value-position choices, for canonical
    /// type-level comparison.
    pub(crate) fn without_ordinary_choices(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            choice: self.choice.filter(|choice| !choice.is_ordinary()),
            delivered: self.delivered.and_then(|list| list.without_ordinary(db)),
            evidence: self.evidence.filter(|ev| !ev.is_ordinary(db)),
            bound_evidence: self.bound_evidence.filter(|ev| !ev.is_ordinary(db)),
            ..self
        }
    }

    /// The trait's default body for this constant, with its declared type and
    /// arguments for this use: the declaration a body chosen later is reached
    /// through.
    pub(crate) fn trait_default_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(
        crate::hir_def::Body<'db>,
        TyId<'db>,
        super::ty_lower::CompleteSubst<'db>,
    )> {
        let trait_ = self.inst.def(db);
        let constant = trait_.const_(db, self.name)?;
        let subst = super::ty_lower::CompleteSubst::for_owner(
            db,
            trait_.into(),
            self.inst.args(db).to_vec(),
        )
        .ok()?;
        Some((constant.default_body(db)?, constant.ty(db)?, subst))
    }

    /// The constant body this use reads, when its implementation is known:
    /// the selected implementation's constant or the trait's default, with
    /// its declared type and that body's arguments (`chosen_constant`).
    pub(crate) fn body_that_runs(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(
        crate::hir_def::Body<'db>,
        TyId<'db>,
        super::ty_lower::CompleteSubst<'db>,
    )> {
        let body = super::trait_def::chosen_constant(db, self)?.body?;
        Some((body.body, body.ty, body.subst))
    }

    /// The slots the implementation selected for this constant declares on its
    /// generic parameters, each as written and as instantiated for this use.
    pub(crate) fn selected_impl_goals(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<(TraitInstId<'db>, TraitInstId<'db>)> {
        let Some(chosen) = super::trait_def::chosen_constant(db, self) else {
            return Vec::new();
        };
        let super::trait_def::ImplementorOrigin::Hir(implementation) =
            chosen.resolved.selected().origin(db)
        else {
            return Vec::new();
        };
        super::trait_choice::impl_declared_goals(db, implementation, chosen.resolved.impl_args(db))
    }
}

impl<'db> TyVisitable<'db> for AssocConstUse<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.inst.visit_with(visitor);
        if let Some(choice) = &self.choice {
            choice.visit_with(visitor);
        }
        if let Some(list) = &self.delivered {
            list.visit_with(visitor);
        }
        if let Some(evidence) = &self.evidence {
            evidence.visit_with(visitor);
        }
        if let Some(bound_evidence) = &self.bound_evidence {
            bound_evidence.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for AssocConstUse<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            origin_scope: self.origin_scope,
            assumptions: self.assumptions.fold_with(db, folder),
            inst: self.inst.fold_with(db, folder),
            name: self.name,
            choice: self.choice.map(|choice| choice.fold_with(db, folder)),
            delivered: self.delivered.map(|list| list.fold_with(db, folder)),
            evidence: self.evidence.map(|ev| ev.fold_with(db, folder)),
            bound_evidence: self.bound_evidence.map(|ev| ev.fold_with(db, folder)),
        }
    }
}

/// A use of an associated const defined in an inherent `impl` block,
/// e.g. `Foo::<7>::SIZE` or `Self::SIZE` inside the impl.
///
/// Carries the receiver type so the impl's generic parameters can be
/// recovered by unifying the impl self type against it once the receiver
/// is concrete (after typed-body instantiation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct InherentConstUse<'db> {
    origin_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    impl_: Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
    /// The decisions for the bounds of the implementation the body is in.
    delivered: Option<DeliveryList<'db>>,
    /// Proofs of the assumptions the inherent constant's body is checked under.
    bound_evidence: Option<super::evidence::EvidenceList<'db>>,
}

impl<'db> InherentConstUse<'db> {
    pub fn new(
        origin_scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        impl_: Impl<'db>,
        receiver_ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self {
            origin_scope,
            assumptions,
            impl_,
            receiver_ty,
            name,
            delivered: None,
            bound_evidence: None,
        }
    }

    pub fn delivered(self) -> Option<DeliveryList<'db>> {
        self.delivered
    }

    pub fn with_delivered(self, delivered: Option<DeliveryList<'db>>) -> Self {
        Self { delivered, ..self }
    }

    pub fn bound_evidence(self) -> Option<super::evidence::EvidenceList<'db>> {
        self.bound_evidence
    }

    pub fn with_bound_evidence(
        self,
        bound_evidence: Option<super::evidence::EvidenceList<'db>>,
    ) -> Self {
        Self {
            bound_evidence,
            ..self
        }
    }

    pub(crate) fn bind_evidence(
        self,
        db: &'db dyn HirAnalysisDb,
        bound: super::evidence::EvidenceList<'db>,
    ) -> Self {
        Self {
            bound_evidence: self.bound_evidence.map(|ev| ev.bind(db, bound)),
            ..self
        }
    }

    /// This use without ordinary value-position choices, for canonical
    /// type-level comparison.
    pub(crate) fn without_ordinary_choices(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            delivered: self.delivered.and_then(|list| list.without_ordinary(db)),
            bound_evidence: self.bound_evidence.filter(|ev| !ev.is_ordinary(db)),
            ..self
        }
    }

    pub fn origin_scope(self) -> ScopeId<'db> {
        self.origin_scope
    }

    pub fn with_env(self, origin_scope: ScopeId<'db>, assumptions: PredicateListId<'db>) -> Self {
        Self {
            origin_scope,
            assumptions,
            ..self
        }
    }

    pub fn assumptions(self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn impl_(self) -> Impl<'db> {
        self.impl_
    }

    pub fn receiver_ty(self) -> TyId<'db> {
        self.receiver_ty
    }

    pub fn name(self) -> IdentId<'db> {
        self.name
    }
}

impl<'db> TyVisitable<'db> for InherentConstUse<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.receiver_ty.visit_with(visitor);
        if let Some(list) = &self.delivered {
            list.visit_with(visitor);
        }
        if let Some(bound_evidence) = &self.bound_evidence {
            bound_evidence.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for InherentConstUse<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            origin_scope: self.origin_scope,
            assumptions: self.assumptions.fold_with(db, folder),
            impl_: self.impl_,
            receiver_ty: self.receiver_ty.fold_with(db, folder),
            name: self.name,
            delivered: self.delivered.map(|list| list.fold_with(db, folder)),
            bound_evidence: self
                .bound_evidence
                .map(|bound_evidence| bound_evidence.fold_with(db, folder)),
        }
    }
}
