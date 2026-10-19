//! Trait choices as values.
//!
//! A choice answers one trait slot (a goal without its associated-type
//! equalities). The checker decides each choice once, at the use that needs
//! it, and stores it on that use. An instance carries the choices its caller
//! delivered to its slots. Every later stage resolves a use with
//! [`SlotChoices::resolve`]; none decides again from source.

use common::indexmap::IndexMap;
use salsa::Update;

use super::{
    binder::Binder,
    fold::{TyFoldable, TyFolder},
    trait_def::{
        CheckedApplicationError, ResolvedImplInstance, TraitInstId, instantiate_selected_impl,
        prove_selected_hir_application,
    },
    trait_lower::{complete_selected_impl, lower_impl_trait},
    trait_resolution::TraitSolveCx,
    ty_def::TyId,
    visitor::{TyVisitable, TyVisitor},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{Func, ImplTrait},
};

/// The slot a goal occupies: its trait and arguments, without associated-type
/// equalities, which constrain the accepted answer rather than name a slot.
pub fn slot_goal<'db>(db: &'db dyn HirAnalysisDb, goal: TraitInstId<'db>) -> TraitInstId<'db> {
    if goal.assoc_type_bindings(db).is_empty() {
        return goal;
    }
    TraitInstId::new(
        db,
        goal.def(db),
        goal.args(db).clone(),
        IndexMap::new(),
        goal.mode(db),
    )
}

/// One implementation for one slot, with its conditions checked where the
/// choice was decided. Identity includes the implementation, the slot, and
/// any decisions recorded for the implementation's own prerequisite bounds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct KnownChoice<'db> {
    implementation: ImplTrait<'db>,
    goal: TraitInstId<'db>,
    bounds: Option<DeliveryList<'db>>,
    ordinary: bool,
}

impl<'db> KnownChoice<'db> {
    pub fn implementation(self) -> ImplTrait<'db> {
        self.implementation
    }

    pub fn goal(self) -> TraitInstId<'db> {
        self.goal
    }

    pub fn bounds(self) -> Option<DeliveryList<'db>> {
        self.bounds
    }

    pub fn is_ordinary(self) -> bool {
        self.ordinary
    }

    /// Check one authored implementation for `goal` in `cx` and name it.
    pub fn prove(
        db: &'db dyn HirAnalysisDb,
        cx: TraitSolveCx<'db>,
        implementation: ImplTrait<'db>,
        goal: TraitInstId<'db>,
    ) -> Result<Self, CheckedApplicationError> {
        prove_selected_hir_application(db, cx, implementation, goal)?;
        Ok(Self {
            implementation,
            goal: slot_goal(db, goal),
            bounds: None,
            ordinary: false,
        })
    }

    /// A named implementation inside its own body: it is the answer
    /// for its own slot there, with its conditions as premises.
    pub fn own(
        db: &'db dyn HirAnalysisDb,
        implementation: ImplTrait<'db>,
        goal: TraitInstId<'db>,
    ) -> Self {
        Self {
            implementation,
            goal: slot_goal(db, goal),
            bounds: None,
            ordinary: false,
        }
    }

    /// An ambient ordinary implementation selected by the checker, together
    /// with the decisions recorded for its prerequisite bounds.
    pub fn ordinary(
        db: &'db dyn HirAnalysisDb,
        implementation: ImplTrait<'db>,
        goal: TraitInstId<'db>,
        bounds: Option<DeliveryList<'db>>,
    ) -> Self {
        let bounds = bounds.filter(|list| !list.items(db).is_empty());
        let ordinary = bounds
            .is_none_or(|list| list.items(db).iter().all(|(_, c)| c.is_ordinary_or_bound()));
        Self {
            implementation,
            goal: slot_goal(db, goal),
            bounds,
            ordinary,
        }
    }

    /// Specialize this choice's prerequisite bound decisions inside an
    /// instance carrying `choices`.
    pub fn specialize(self, db: &'db dyn HirAnalysisDb, choices: &SlotChoices<'db>) -> Self {
        let Some(bounds) = self.bounds else {
            return self;
        };
        let bounds = bounds.specialize(db, choices);
        let bounds = (!bounds.items(db).is_empty()).then_some(bounds);
        let ordinary = self.ordinary
            && bounds
                .is_none_or(|list| list.items(db).iter().all(|(_, c)| c.is_ordinary_or_bound()));
        Self {
            bounds,
            ordinary,
            ..self
        }
    }

    /// The chosen implementation applied to `inst`, a goal in this slot, for
    /// its outputs (`answers`). A body is reached through
    /// `trait_def::chosen_method` and `chosen_constant`, with its choices.
    fn resolved_instance(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Option<ResolvedImplInstance<'db>> {
        let candidate = complete_selected_impl(db, lower_impl_trait(db, self.implementation)?)?;
        instantiate_selected_impl(db, candidate, inst)
    }

    /// Whether this choice answers `goal`: the same slot, and the chosen
    /// implementation's associated types are the goal's equalities. The one
    /// test of a known choice against a requested goal.
    pub fn answers(self, db: &'db dyn HirAnalysisDb, goal: TraitInstId<'db>) -> bool {
        if slot_goal(db, goal) != self.goal {
            return false;
        }
        let bindings = goal.assoc_type_bindings(db);
        if bindings.is_empty() {
            return true;
        }
        let Some(resolved) = self.resolved_instance(db, self.goal) else {
            return false;
        };
        bindings
            .iter()
            .all(|(name, ty)| resolved.instantiated_assoc_ty(db, *name) == Some(*ty))
    }
}

impl<'db> TyFoldable<'db> for KnownChoice<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self {
            implementation: self.implementation,
            goal: self.goal.fold_with(db, folder),
            bounds: self.bounds.map(|bounds| bounds.fold_with(db, folder)),
            ordinary: self.ordinary,
        }
    }
}

impl<'db> TyVisitable<'db> for KnownChoice<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.goal.visit_with(visitor);
        if let Some(bounds) = self.bounds {
            bounds.visit_with(visitor);
        }
    }
}

/// A place in an owner's signature through which a caller delivers a choice.
/// It names the place, so substitution never rewrites it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SlotRef<'db> {
    /// The owner's `index`-th value parameter, of type `Evidence<G>`.
    EvidenceParam(u16),
    /// One of the owner's assumptions, as written in its own generic context.
    Bound(TraitInstId<'db>),
    /// A slot the caller had no choice for: it received it from an evidence
    /// parameter that it was not given itself. Never answered the ordinary
    /// way; only an instance's specialized uses carry it.
    Unsupplied,
}

/// Where a call delivers a choice: an evidence parameter by index, a bound of
/// the body the checker knew at the call by that body's name for it, or, for a
/// body chosen later, a bound of the callee's declaration by its name there,
/// which reaches the body that runs through `role_map`. Bound names are in
/// their declarations' contexts, so substitution never rewrites a delivery,
/// and two bounds that coincide only after substitution stay apart.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum Delivery<'db> {
    Param(u16),
    Bound(TraitInstId<'db>),
    Nominal(TraitInstId<'db>),
}

impl<'db> TyFoldable<'db> for Delivery<'db> {
    fn super_fold_with<F: TyFolder<'db>>(
        self,
        _db: &'db dyn HirAnalysisDb,
        _folder: &mut F,
    ) -> Self {
        self
    }
}

impl<'db> TyVisitable<'db> for Delivery<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, _visitor: &mut V) {}
}

/// The slots `func` declares, each as written in its own generic context and
/// as instantiated with `args`: every bound on its generic parameters,
/// including, for a trait method, the trait's `Self` bound and its
/// supertraits. A call's choices reach a body only through these and its
/// evidence parameters; bounds on concrete types are not slots.
pub fn declared_goals<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    args: &[TyId<'db>],
) -> Vec<(TraitInstId<'db>, TraitInstId<'db>)> {
    let mut goals = Vec::new();
    for own in own_slots(db, func) {
        let at_call = slot_goal(db, Binder::bind(func.into(), own).instantiate(db, args));
        if !goals.contains(&(own, at_call)) {
            goals.push((own, at_call));
        }
    }
    goals
}

/// The slots `implementation` declares on its generic parameters, each as
/// written in its own generic context and as instantiated with `args`.
pub fn impl_declared_goals<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
    args: &[TyId<'db>],
) -> Vec<(TraitInstId<'db>, TraitInstId<'db>)> {
    let mut goals = Vec::new();
    for own in impl_slots(db, implementation) {
        let at_use = slot_goal(
            db,
            Binder::bind(
                crate::hir_def::GenericParamOwner::ImplTrait(implementation),
                own,
            )
            .instantiate(db, args),
        );
        if !goals.contains(&(own, at_use)) {
            goals.push((own, at_use));
        }
    }
    goals
}

fn impl_slots<'db>(db: &'db dyn HirAnalysisDb, implementation: ImplTrait<'db>) -> Vec<TraitInstId<'db>> {
    let bounds = super::trait_resolution::constraint::collect_constraints(
        db,
        crate::hir_def::GenericParamOwner::ImplTrait(implementation),
    )
    .instantiate_identity();
    slots_of(db, bounds.extend_all_bounds(db))
}

/// The slots `func` declares, as written in its own generic context: its
/// bounds, and the bounds its trait-keyed effect requirements put on their
/// providers: a trait provider is a bound in another form.
pub fn own_slots<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Vec<TraitInstId<'db>> {
    own_slots_query(db, func).clone()
}

#[salsa::tracked(return_ref)]
fn own_slots_query<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Vec<TraitInstId<'db>> {
    let mut bounds = crate::semantic::func_body_assumptions(db, func)
        .list(db)
        .to_vec();
    for provider in
        super::trait_resolution::constraint::collect_func_trait_provider_constraints(db, func)
    {
        if !bounds.contains(&provider) {
            bounds.push(provider);
        }
    }
    slots_of(
        db,
        super::trait_resolution::PredicateListId::new(db, bounds).extend_all_bounds(db),
    )
}

/// The slots a constant body declares, as written: the bounds its item (a
/// trait or an implementation) puts on its generic parameters, with a trait's
/// `Self` and supertraits. The constant counterpart of `own_slots`; the
/// checker answers a body's bound slots from the same list.
pub fn const_body_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
) -> Vec<TraitInstId<'db>> {
    slots_of(
        db,
        super::const_ty::const_body_assumptions(db, body.scope()).extend_all_bounds(db),
    )
}

/// The slots of a constant body, each as written and as instantiated with
/// `subst` at a use.
pub fn const_declared_goals<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    subst: &super::ty_lower::CompleteSubst<'db>,
) -> Vec<(TraitInstId<'db>, TraitInstId<'db>)> {
    const_body_slots(db, body)
        .into_iter()
        .filter_map(|own| {
            let at_use = super::subst::substitute_complete(db, own, subst).ok()?;
            Some((own, slot_goal(db, at_use)))
        })
        .collect()
}

fn slots_of<'db>(
    db: &'db dyn HirAnalysisDb,
    assumptions: super::trait_resolution::PredicateListId<'db>,
) -> Vec<TraitInstId<'db>> {
    let mut slots = Vec::new();
    for bound in assumptions.list(db).iter().copied() {
        if bound.args(db).iter().any(|ty| ty.has_param(db)) {
            let own = slot_goal(db, bound);
            if !slots.contains(&own) {
                slots.push(own);
            }
        }
    }
    slots
}

/// The decisions a use of a constant delivers to the constant body that runs,
/// by that body's bound names. Interned, so a constant use stays a value.
#[salsa::interned]
#[derive(Debug)]
pub struct DeliveryList<'db> {
    #[return_ref]
    pub items: Vec<(Delivery<'db>, Choice<'db>)>,
}

impl<'db> DeliveryList<'db> {
    /// The list with each choice read inside an instance carrying `choices`:
    /// a known choice stays, a pending one becomes `Unsupplied`, an ordinary
    /// one is left out.
    pub fn specialize(self, db: &'db dyn HirAnalysisDb, choices: &SlotChoices<'db>) -> Self {
        let items: Vec<(Delivery<'db>, Choice<'db>)> = self
            .items(db)
            .iter()
            .filter_map(|(delivery, choice)| {
                let choice = match choices.resolve_in(db, Some(*choice)) {
                    Resolved::Known(known) => Choice::Known(known),
                    Resolved::Pending => Choice::Slot(SlotRef::Unsupplied),
                    Resolved::Ordinary => return None,
                };
                Some((*delivery, choice))
            })
            .collect();
        Self::new(db, items)
    }

    /// What this list delivers to the slots of `body`, the constant body
    /// that runs, by that body's bound names. The one mapping from a constant
    /// use to the body that runs.
    pub fn const_body_deliveries(
        self,
        db: &'db dyn HirAnalysisDb,
        body: crate::hir_def::Body<'db>,
    ) -> Vec<(TraitInstId<'db>, Choice<'db>)> {
        let slots = const_body_slots(db, body);
        self.items(db)
            .iter()
            .filter_map(|(delivery, choice)| match delivery {
                // A constant's declaration is its trait's default body, so a
                // declaration's name reaches that body as its own; an
                // implementation's constant declares its own bounds.
                Delivery::Bound(bound) | Delivery::Nominal(bound) if slots.contains(bound) => {
                    Some((*bound, *choice))
                }
                _ => None,
            })
            .collect()
    }

    /// The instance choices a constant body receives from this list.
    pub fn to_body(
        self,
        db: &'db dyn HirAnalysisDb,
        body: crate::hir_def::Body<'db>,
    ) -> SlotChoices<'db> {
        SlotChoices::from_resolved(
            self.const_body_deliveries(db, body)
                .into_iter()
                .map(|(bound, choice)| {
                    (
                        SlotRef::Bound(bound),
                        SlotChoices::default().resolve(Some(choice)),
                    )
                })
                .collect(),
        )
    }
}

impl<'db> TyFoldable<'db> for DeliveryList<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        let items: Vec<(Delivery<'db>, Choice<'db>)> = self
            .items(db)
            .iter()
            .map(|(delivery, choice)| (*delivery, choice.fold_with(db, folder)))
            .collect();
        Self::new(db, items)
    }
}

impl<'db> TyVisitable<'db> for DeliveryList<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        for (_, choice) in self.items(visitor.db()) {
            choice.visit_with(visitor);
        }
    }
}

/// What a call of `nominal` delivers to the slots of `body`, the body that
/// runs: a parameter by its index, a bound of `body` by name (including any
/// prerequisite bounds carried by `chosen`), and a bound of `nominal` through
/// `role_map`. The one mapping from a call to the body that runs.
pub fn body_deliveries<'db>(
    db: &'db dyn HirAnalysisDb,
    chosen: Option<KnownChoice<'db>>,
    delivered: &[(Delivery<'db>, Choice<'db>)],
    nominal: Func<'db>,
    body: Func<'db>,
) -> Vec<(SlotRef<'db>, Choice<'db>)> {
    let mut slots: Vec<(SlotRef<'db>, Choice<'db>)> = Vec::new();
    let give = |slot: SlotRef<'db>, choice: Choice<'db>, slots: &mut Vec<_>| {
        if !slots.iter().any(|(given, _)| *given == slot) {
            slots.push((slot, choice));
        }
    };
    let chosen_bounds = chosen
        .and_then(|known| known.bounds())
        .map(|list| list.items(db).as_slice())
        .unwrap_or(&[]);
    let own = if delivered
        .iter()
        .chain(chosen_bounds.iter())
        .any(|(delivery, _)| matches!(delivery, Delivery::Bound(_)))
    {
        own_slots(db, body)
    } else {
        Vec::new()
    };
    for (delivery, choice) in delivered.iter().chain(chosen_bounds.iter()) {
        match delivery {
            Delivery::Param(idx) => give(SlotRef::EvidenceParam(*idx), *choice, &mut slots),
            Delivery::Bound(bound) if own.contains(bound) => {
                give(SlotRef::Bound(*bound), *choice, &mut slots)
            }
            Delivery::Bound(bound) => {
                let mut_bound = bound.at_mode(db, Some(super::BorrowKind::Mut));
                if own.contains(&mut_bound) {
                    give(SlotRef::Bound(mut_bound), *choice, &mut slots);
                }
            }
            Delivery::Nominal(bound) => {
                if let Some((_, in_body)) = role_map(db, nominal, body)
                    .iter()
                    .find(|(declared, _)| declared == bound)
                {
                    give(SlotRef::Bound(*in_body), *choice, &mut slots);
                }
            }
        }
    }
    slots
}

/// The bounds of a trait method `nominal` that a body `body` chosen for it
/// also declares, each as `(nominal's name, body's name)`. The nominal bound
/// is rewritten into the body's own generic context by role: the trait's
/// parameters as the implementation's header writes them, the method's own
/// parameters by position. A default body is the nominal itself.
#[salsa::tracked(return_ref)]
pub fn role_map<'db>(
    db: &'db dyn HirAnalysisDb,
    nominal: Func<'db>,
    body: Func<'db>,
) -> Vec<(TraitInstId<'db>, TraitInstId<'db>)> {
    use super::ty_lower::collect_generic_params;
    use crate::hir_def::{GenericParamOwner, ItemKind};
    if nominal == body {
        return own_slots(db, nominal)
            .into_iter()
            .map(|bound| (bound, bound))
            .collect();
    }
    let Some(ItemKind::ImplTrait(implementation)) = body.scope().parent_item(db) else {
        return Vec::new();
    };
    let Some(header) = lower_impl_trait(db, implementation) else {
        return Vec::new();
    };
    let header = header.trait_inst(db);
    let nominal_params = collect_generic_params(db, GenericParamOwner::Func(nominal)).params(db);
    let body_params = collect_generic_params(db, GenericParamOwner::Func(body)).params(db);
    let inherited = collect_generic_params(db, GenericParamOwner::ImplTrait(implementation))
        .params(db)
        .len();
    let trait_args = header.args(db);
    if trait_args.len() > nominal_params.len()
        || body_params.len() < inherited
        || nominal_params.len() - trait_args.len() != body_params.len() - inherited
    {
        return Vec::new();
    }
    let in_body_context = trait_args
        .iter()
        .chain(&body_params[inherited..])
        .copied()
        .collect::<Vec<_>>();
    let body_slots = own_slots(db, body);
    own_slots(db, nominal)
        .into_iter()
        .filter_map(|bound| {
            let in_body = slot_goal(
                db,
                Binder::bind(nominal.into(), bound).instantiate(db, &in_body_context),
            );
            body_slots.contains(&in_body).then_some((bound, in_body))
        })
        .collect()
}

/// Why a site may select an implementation by ordinary selection alone,
/// without reading a recorded choice. The ordinary selection
/// (`resolve_trait_impl_instance`) and method lookup
/// (`resolve_trait_method_instance`) take one. Only this module makes them,
/// one constructor per argument, so every caller names
/// its argument and a guard checks where each is used.
#[derive(Clone, Copy, Debug)]
pub struct OrdinarySelection(());

impl OrdinarySelection {
    /// A use for which the checker recorded no decision that names an
    /// implementation: it means the ordinary implementation (a pending
    /// choice is a recorded decision and never comes here).
    pub(super) fn no_recorded_choice() -> Self {
        Self(())
    }

    /// A slot's associated types, effect rows and result spaces, equal for
    /// every implementation whose header overlaps.
    pub(super) fn slot_outputs() -> Self {
        Self(())
    }

    /// A site only the outermost tier answers today, because no block frame
    /// reaches it: declarations, storage and encoding, the ABI, generated
    /// code. A module-level frame would reach some of them; each use is a
    /// site to revisit then.
    pub(super) fn outermost_tier_only() -> Self {
        Self(())
    }

    /// A check that decides no choice: it may only refuse, or compare with a
    /// body already selected.
    pub(crate) fn decides_nothing() -> Self {
        Self(())
    }

    /// A trait constant read in a type. Every type that would read a slot a
    /// frame answers is refused, so the outermost tier's value is the only
    /// one a type can mean.
    pub(super) fn outermost_tier_type() -> Self {
        Self(())
    }

    /// Tests that inspect ordinary selection directly.
    pub fn for_tests() -> Self {
        Self(())
    }
}

/// Why the checker refuses a trait use after inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ChoiceRefusal {
    /// It passes ordinary evidence for a bound whose implementation has a
    /// condition on a slot opened here (a conditional implementation).
    OpenedCondition,
    /// It passes ordinary evidence for a bound whose proof cannot be followed
    /// while a slot is opened here (no single implementation, or a walk past
    /// its limits), so it is not known to avoid the opened slot.
    UnfollowedProof,
    /// A type its callee declares (an argument, the result, an effect's
    /// key) reads a constant of a slot opened here.
    DependentType,
    /// A `with` operand whose type was not known where it was checked turned
    /// out to be evidence: it was taken as an effect value and opened
    /// nothing.
    UninferredEvidence,
    /// A closure reads a choice a frame answers and needs an effect provider
    /// where it is called: the choice is read where the closure is written,
    /// the provider where it is called, and the two could disagree.
    ClosureChoiceAndProvider,
}

/// What a trait use means: a known implementation, or whatever the enclosing
/// instance receives through one of its slots.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum Choice<'db> {
    Known(KnownChoice<'db>),
    Slot(SlotRef<'db>),
}

impl<'db> Choice<'db> {
    /// The evidence parameter this choice waits for, if it is one.
    pub fn evidence_param(self) -> Option<u16> {
        match self {
            Self::Slot(SlotRef::EvidenceParam(idx)) => Some(idx),
            _ => None,
        }
    }

    pub fn known(self) -> Option<KnownChoice<'db>> {
        match self {
            Self::Known(known) => Some(known),
            Self::Slot(_) => None,
        }
    }

    pub fn is_ordinary(self) -> bool {
        matches!(self, Self::Known(known) if known.is_ordinary())
    }

    pub fn is_ordinary_or_bound(self) -> bool {
        self.is_ordinary() || matches!(self, Self::Slot(SlotRef::Bound(_)))
    }
}

impl<'db> TyFoldable<'db> for Choice<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        match self {
            Self::Known(choice) => Self::Known(choice.fold_with(db, folder)),
            Self::Slot(slot) => Self::Slot(slot),
        }
    }
}

impl<'db> TyVisitable<'db> for Choice<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        if let Self::Known(choice) = self {
            choice.visit_with(visitor);
        }
    }
}

/// The choices delivered to one instance, in the owner's slot order: a known
/// implementation, or `None` when the caller's own choice was pending. An
/// absent bound is answered the ordinary way; an absent evidence parameter is
/// pending (see [`SlotChoices::resolve`]).
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Update)]
pub struct SlotChoices<'db> {
    slots: Vec<(SlotRef<'db>, Option<KnownChoice<'db>>)>,
}

impl<'db> SlotChoices<'db> {
    pub fn new(choices: Vec<(SlotRef<'db>, KnownChoice<'db>)>) -> Self {
        Self {
            slots: choices
                .into_iter()
                .map(|(slot, choice)| (slot, Some(choice)))
                .collect(),
        }
    }

    /// Deliveries resolved in the caller: known, ordinary (left out), or
    /// pending (kept, so that the callee does not read them as ordinary).
    pub fn from_resolved(choices: Vec<(SlotRef<'db>, Resolved<'db>)>) -> Self {
        Self {
            slots: choices
                .into_iter()
                .filter_map(|(slot, resolved)| match resolved {
                    Resolved::Known(known) => Some((slot, Some(known))),
                    Resolved::Pending => Some((slot, None)),
                    Resolved::Ordinary => None,
                })
                .collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    pub fn is_ordinary(&self) -> bool {
        self.slots
            .iter()
            .all(|(_, choice)| choice.is_some_and(|known| known.is_ordinary()))
    }

    pub fn iter(&self) -> impl Iterator<Item = (SlotRef<'db>, Option<KnownChoice<'db>>)> + '_ {
        self.slots.iter().copied()
    }

    /// The choice delivered to `slot`: `Some(None)` when it was pending.
    pub fn get(&self, slot: SlotRef<'db>) -> Option<Option<KnownChoice<'db>>> {
        self.slots
            .iter()
            .find_map(|(key, choice)| (*key == slot).then_some(*choice))
    }

    /// The meaning of a use's recorded decision inside the instance that
    /// carries these slot choices. Ordinary only for a use with no decision
    /// or an undelivered bound; an undelivered evidence parameter stays
    /// pending and never resolves the ordinary way.
    pub fn resolve(&self, choice: Option<Choice<'db>>) -> Resolved<'db> {
        match choice {
            None => Resolved::Ordinary,
            Some(Choice::Known(known)) => Resolved::Known(known),
            Some(Choice::Slot(slot)) => match self.get(slot) {
                Some(Some(known)) => Resolved::Known(known),
                Some(None) => Resolved::Pending,
                None if matches!(slot, SlotRef::Bound(_)) => Resolved::Ordinary,
                None => Resolved::Pending,
            },
        }
    }

    /// Resolve `choice` inside `self` and specialize any prerequisite bound
    /// decisions carried by a resulting [`KnownChoice`] with `self`.
    pub fn resolve_in(
        &self,
        db: &'db dyn HirAnalysisDb,
        choice: Option<Choice<'db>>,
    ) -> Resolved<'db> {
        match self.resolve(choice) {
            Resolved::Known(known) => Resolved::Known(known.specialize(db, self)),
            resolved => resolved,
        }
    }
}

/// A decision read inside one instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Resolved<'db> {
    Known(KnownChoice<'db>),
    /// The outermost tier answers the use: today the
    /// ordinary implementation. Once ambient implementations are gone, an
    /// unanswered slot is a refusal, never a guess.
    Ordinary,
    /// An evidence parameter this instance was not given: no stage may read
    /// it as the ordinary implementation.
    Pending,
}

impl<'db> Resolved<'db> {
    pub fn known(self) -> Option<KnownChoice<'db>> {
        match self {
            Self::Known(known) => Some(known),
            Self::Ordinary | Self::Pending => None,
        }
    }
}

impl<'db> TyFoldable<'db> for SlotChoices<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self {
            slots: self
                .slots
                .into_iter()
                .map(|(slot, choice)| (slot, choice.map(|choice| choice.fold_with(db, folder))))
                .collect(),
        }
    }
}

impl<'db> TyVisitable<'db> for SlotChoices<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        for (_, choice) in &self.slots {
            if let Some(choice) = choice {
                choice.visit_with(visitor);
            }
        }
    }
}

/// `ty` with every constant replaced by an invalid one: the recovery type for
/// a refused choice-dependent type, which keeps its shape and element types.
pub fn invalidate_consts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: super::ty_def::TyId<'db>,
) -> super::ty_def::TyId<'db> {
    struct Invalidate;
    impl<'db> TyFolder<'db> for Invalidate {
        fn fold_ty(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            ty: super::ty_def::TyId<'db>,
        ) -> super::ty_def::TyId<'db> {
            match ty.data(db) {
                // A constant refused where it is read already stands for the
                // refusal, and unifies with any constant (`unify`), so the
                // code around it still infers.
                super::ty_def::TyData::ConstTy(const_ty)
                    if super::ty_check::written_frames::refused_at_read(db, *const_ty) =>
                {
                    ty
                }
                super::ty_def::TyData::ConstTy(_) => super::ty_def::TyId::const_ty(
                    db,
                    super::const_ty::ConstTyId::invalid(db, super::ty_def::InvalidCause::Other),
                ),
                _ => ty.super_fold_with(db, self),
            }
        }
    }
    ty.fold_with(db, &mut Invalidate)
}

#[cfg(test)]
mod tests {
    use super::{Choice, Resolved, SlotChoices, SlotRef};

    #[test]
    fn a_pending_evidence_choice_never_resolves_ordinary() {
        // Undelivered evidence parameter: pending, not ordinary.
        let none = SlotChoices::default();
        assert_eq!(
            none.resolve(Some(Choice::Slot(SlotRef::EvidenceParam(0)))),
            Resolved::Pending
        );
        assert_eq!(
            none.resolve(Some(Choice::Slot(SlotRef::Unsupplied))),
            Resolved::Pending
        );
        // Delivered as pending by a caller that had no choice itself.
        let pending =
            SlotChoices::from_resolved(vec![(SlotRef::EvidenceParam(1), Resolved::Pending)]);
        assert_eq!(
            pending.resolve(Some(Choice::Slot(SlotRef::EvidenceParam(1)))),
            Resolved::Pending
        );
        // No decision is the only ordinary case without a bound.
        assert_eq!(none.resolve(None), Resolved::Ordinary);
    }
}
