use salsa::Update;

use super::ty_def::TyId;
use super::{
    fold::{TyFoldable, TyFolder},
    trait_choice::{Choice, DeliveryList, Resolved, SlotChoices, SlotRef},
    trait_def::TraitInstId,
    trait_def::{ResolvedImplInstance, resolve_trait_impl_instance},
    trait_resolution::{PredicateListId, Selection, TraitSolveCx},
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
    /// `None` in a type: the core refuses types that depend on a choice.
    choice: Option<Choice<'db>>,
    /// The decisions for the bounds of the constant body that runs.
    delivered: Option<DeliveryList<'db>>,
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
        }
    }

    pub fn choice(self) -> Option<Choice<'db>> {
        self.choice
    }

    pub fn delivered(self) -> Option<DeliveryList<'db>> {
        self.delivered
    }

    pub fn with_delivered(self, delivered: Option<DeliveryList<'db>>) -> Self {
        Self { delivered, ..self }
    }

    pub fn with_choice(self, choice: Option<Choice<'db>>) -> Self {
        Self { choice, ..self }
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

    /// The implementation this use reads: its recorded choice when that is
    /// known, ordinary selection when there is no decision or the decision is
    /// the body's own bound, and nothing for a pending choice, which is never
    /// answered the ordinary way (L1). The only resolver for value uses.
    pub(crate) fn selected_impl(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Selection<ResolvedImplInstance<'db>> {
        match self.choice {
            Some(Choice::Known(known)) => known
                .resolved_instance(
                    db,
                    super::trait_resolution::normalize_trait_inst_preserving_validity(
                        db,
                        self.inst,
                        self.origin_scope,
                        self.assumptions,
                    ),
                )
                .map_or(Selection::NotFound, Selection::Unique),
            Some(Choice::Slot(SlotRef::Bound(_))) | None => resolve_trait_impl_instance(
                db,
                self.solve_cx(db),
                self.inst,
                crate::analysis::ty::trait_choice::OrdinarySelection::no_recorded_choice(),
            ),
            Some(Choice::Slot(_)) => Selection::NotFound,
        }
    }

    /// This use read inside an instance that carries `choices`: its own
    /// decision and the decisions it delivers to the constant body.
    pub(crate) fn specialize(self, db: &'db dyn HirAnalysisDb, choices: &SlotChoices<'db>) -> Self {
        let delivered = self.delivered.map(|list| list.specialize(db, choices));
        let this = Self { delivered, ..self };
        if !matches!(this.choice, Some(Choice::Slot(_))) {
            return this;
        }
        match choices.resolve(this.choice) {
            Resolved::Known(known) => this.with_choice(Some(Choice::Known(known))),
            Resolved::Pending => this.with_choice(Some(Choice::Slot(SlotRef::Unsupplied))),
            Resolved::Ordinary => this,
        }
    }

    /// The trait's default body for this constant, with arguments for this
    /// use: the declaration a body chosen later is reached through.
    pub(crate) fn trait_default_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(
        crate::hir_def::Body<'db>,
        super::ty_lower::CompleteSubst<'db>,
    )> {
        let trait_ = self.inst.def(db);
        let body = trait_.const_(db, self.name)?.default_body(db)?;
        let subst = super::ty_lower::CompleteSubst::for_owner(
            db,
            trait_.into(),
            self.inst.args(db).to_vec(),
        )
        .ok()?;
        Some((body, subst))
    }

    /// The constant body this use reads, when its implementation is known:
    /// the selected implementation's constant or the trait's default, with
    /// that body's arguments.
    pub(crate) fn body_that_runs(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(
        crate::hir_def::Body<'db>,
        super::ty_lower::CompleteSubst<'db>,
    )> {
        let Selection::Unique(resolved) = self.selected_impl(db) else {
            return None;
        };
        let (body, _, subst) =
            super::trait_def::selected_assoc_const_body_template(db, resolved, self.name)?;
        Some((body, subst))
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
        }
    }

    pub fn delivered(self) -> Option<DeliveryList<'db>> {
        self.delivered
    }

    pub fn with_delivered(self, delivered: Option<DeliveryList<'db>>) -> Self {
        Self { delivered, ..self }
    }

    /// This use read inside an instance that carries `choices`.
    pub(crate) fn specialize(self, db: &'db dyn HirAnalysisDb, choices: &SlotChoices<'db>) -> Self {
        Self {
            delivered: self.delivered.map(|list| list.specialize(db, choices)),
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
        }
    }
}
