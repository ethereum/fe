//! The checker's side of trait choices.
//!
//! `with` maps slots to choices for its body. [`TyChecker::denote_evidence`]
//! is the only function that gives an evidence expression its meaning, so it
//! is also the only place that admits or refuses an evidence source. Each
//! trait use records one decision when the body is finished, from the frame it
//! was checked in.

use num_bigint::BigInt;

use super::{
    Callable, ConstRef, ExprProp, LocalBinding, SemanticExprLowering, TyChecker,
    env::{ParamSite, TyCheckEnv},
    expr::resolve_ident_expr,
    path::ResolvedPathInBody,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            EvalOutcome, SemConstId, SemConstValue, SemOrigin, SemanticInstanceKey, bool_const,
            eval_const_ref, eval_instance_with_args,
            instance::{checked_call_key, resolve_semantic_const_ref},
            int_const,
        },
        ty::{
            const_expr::ConstExpr,
            const_ty::{ConstTyData, ConstTyId, LoweringContext},
            corelib::evidence_goal,
            diagnostics::BodyDiag,
            fold::{TyFoldable, TyFolder},
            trait_choice::{
                Choice, ChoiceRefusal, Delivery, DeliveryList, KnownChoice, SlotRef,
                body_deliveries, const_declared_goals, declared_goals, own_slots, role_map,
                slot_goal,
            },
            trait_def::{
                ImplementorOrigin, TraitInstId, named_impl_candidates, resolve_trait_impl_instance,
            },
            trait_lower::lower_impl_trait,
            trait_resolution::{
                GoalSatisfiability, Selection, constraint::collect_constraints, is_goal_satisfiable,
            },
            ty_def::{TyData, TyId},
            visitor::{TyVisitable, TyVisitor, walk_const_ty},
        },
    },
    hir_def::{CallableDef, Expr, ExprId, LitKind, Partial, PatId},
    span::DynLazySpan,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct FrameId(u32);

struct FrameNode<'db> {
    parent: Option<FrameId>,
    slot: TraitInstId<'db>,
    choice: Choice<'db>,
}

/// The lexical frames of choices, kept as a persistent list so that a use can
/// remember the frame it was checked in and be decided after inference.
#[derive(Default)]
pub(super) struct ChoiceFrames<'db> {
    nodes: Vec<FrameNode<'db>>,
    current: Option<FrameId>,
}

impl<'db> ChoiceFrames<'db> {
    pub(super) fn current(&self) -> Option<FrameId> {
        self.current
    }

    pub(super) fn restore(&mut self, frame: Option<FrameId>) {
        self.current = frame;
    }

    /// The frame a body starts in: inside `impl T for S as N`, the slot
    /// `T<S>` is N (RD114, provisional reading (a): every use of that slot in
    /// the body, `Self` forms, receiver calls and concrete paths alike).
    pub(super) fn seed(&mut self, slot: TraitInstId<'db>, choice: Choice<'db>) {
        debug_assert!(
            self.current.is_none(),
            "a body is seeded before it is checked"
        );
        self.nodes.push(FrameNode {
            parent: None,
            slot,
            choice,
        });
        self.current = Some(FrameId(self.nodes.len() as u32 - 1));
    }

    /// Map `slot` to `choice` in the header that began at `header`. A slot given
    /// twice in one header is an error; an outer frame's slot is shadowed.
    fn push(
        &mut self,
        header: Option<FrameId>,
        slot: TraitInstId<'db>,
        choice: Choice<'db>,
    ) -> Result<(), ()> {
        let mut at = self.current;
        while at != header {
            let node = &self.nodes[at.expect("header is an ancestor").0 as usize];
            if node.slot == slot {
                return Err(());
            }
            at = node.parent;
        }
        self.nodes.push(FrameNode {
            parent: self.current,
            slot,
            choice,
        });
        self.current = Some(FrameId(self.nodes.len() as u32 - 1));
        Ok(())
    }

    /// The frame entry a use of `slot` reads: the innermost entry for exactly
    /// that slot, as written. A slot that would equal an opened one only for
    /// some generic arguments is a different slot (RD71's written reading).
    fn lookup(&self, frame: Option<FrameId>, slot: TraitInstId<'db>) -> Option<Choice<'db>> {
        let mut at = frame;
        while let Some(id) = at {
            let node = &self.nodes[id.0 as usize];
            if node.slot == slot {
                return Some(node.choice);
            }
            at = node.parent;
        }
        None
    }
}

/// Why an expression does not denote a choice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SourceError {
    /// Not a compile-time value (RD116, RD121).
    Runtime,
    /// A compile-time form outside the core.
    Unsupported(&'static str),
    /// Already refused where the source was written; nothing more to report.
    Reported,
}

/// Whether `expr` calls through a path naming the function, rather than
/// through a value; `is_binding` says whether an expression names a local or
/// a parameter. Any other expression than a call counts as direct (a method
/// call, an operator). The one classification of call edges.
pub(super) fn calls_directly<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    expr: ExprId,
    is_binding: impl Fn(ExprId) -> bool,
) -> bool {
    match expr.data(db, body) {
        Partial::Present(Expr::Call(callee, _)) => {
            matches!(callee.data(db, body), Partial::Present(Expr::Path(..)))
                && !is_binding(*callee)
        }
        _ => true,
    }
}

/// Every trait use a checked body records, as the goal it reads at the use
/// and the decision recorded for it: each trait constant, each trait call's
/// own slot, and each goal a call's callee declares, with the choice the call
/// delivers to it. The one answer to "which slots does this body read", for
/// conditions (the decisions) and for types (the goals).
pub(crate) fn body_slot_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &super::TypedBody<'db>,
) -> Vec<(TraitInstId<'db>, Option<Choice<'db>>)> {
    let mut uses = Vec::new();
    let mut note = |goal: TraitInstId<'db>, choice: Option<Choice<'db>>| {
        let goal = slot_goal(db, goal);
        if !uses.contains(&(goal, choice)) {
            uses.push((goal, choice));
        }
    };
    for cref in typed.const_refs() {
        for (goal, choice) in const_use_reads(db, cref) {
            note(goal, choice);
        }
    }
    let Some(body) = typed.body() else {
        return uses;
    };
    for expr in body.exprs(db).keys() {
        let Some(callable) = typed.callable_expr(expr) else {
            continue;
        };
        if let Some(inst) = callable.trait_inst {
            note(inst, callable.choice());
        }
        let CallableDef::Func(func) = callable.callable_def() else {
            continue;
        };
        // The bounds of the body that runs, when it is known here, else the
        // callee's declared ones, each with what the call delivers to it.
        let (body_func, body_args) = callable
            .called_body(db, body.scope(), typed.assumptions())
            .unwrap_or_else(|| (func, callable.generic_args().to_vec()));
        let slots = body_deliveries(db, callable.delivered(), func, body_func);
        for (bound, goal) in declared_goals(db, body_func, &body_args) {
            let delivered = slots
                .iter()
                .find_map(|(slot, choice)| (*slot == SlotRef::Bound(bound)).then_some(*choice));
            note(goal, delivered);
        }
    }
    uses
}

/// The slots a constant use reads, each with its recorded decision: a trait
/// constant's own slot, and the bounds of the constant body that runs (as
/// that body's goals at the use), as `body_slot_uses` reads a call.
pub(crate) fn const_use_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: ConstRef<'db>,
) -> Vec<(TraitInstId<'db>, Option<Choice<'db>>)> {
    let (own, body, subst, delivered) = match cref {
        ConstRef::TraitConst(assoc) => {
            let own = Some((assoc.inst(), assoc.choice()));
            // While inference runs, a goal can still hold inference variables;
            // its body is looked up once they are resolved.
            if assoc.inst().args(db).iter().any(|ty| ty.has_var(db)) {
                return own.into_iter().collect();
            }
            let Some((body, subst)) = assoc.body_that_runs(db) else {
                return own.into_iter().collect();
            };
            (own, body, subst, assoc.delivered())
        }
        ConstRef::InherentConst(use_) => {
            if use_.receiver_ty().has_var(db) {
                return Vec::new();
            }
            let Some((body, impl_args)) =
                crate::analysis::ty::const_ty::inherent_const_body_and_impl_args(
                    db,
                    use_.impl_(),
                    use_.receiver_ty(),
                    use_.name(),
                )
            else {
                return Vec::new();
            };
            let Ok(subst) = crate::analysis::ty::ty_lower::CompleteSubst::for_owner(
                db,
                use_.impl_().into(),
                impl_args,
            ) else {
                return Vec::new();
            };
            (None, body, subst, use_.delivered())
        }
        ConstRef::Const(_) => return Vec::new(),
    };
    let mut reads = own.into_iter().collect::<Vec<_>>();
    let given = delivered.map_or_else(Vec::new, |list| list.const_body_deliveries(db, body));
    for (bound, at_use) in const_declared_goals(db, body, &subst) {
        let choice = given
            .iter()
            .find_map(|(given, choice)| (*given == bound).then_some(*choice));
        reads.push((at_use, choice));
    }
    reads
}

/// The slots whose associated constants a type reads: through trait constant
/// terms, through the anonymous constant bodies it contains (an array length
/// written as an expression), and through the conditions and field types of
/// the records and enums it names.
pub(crate) fn type_slot_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<TraitInstId<'db>> {
    let mut reads = Vec::new();
    type_slot_reads_into(db, ty, &mut Vec::new(), &mut reads);
    reads
}

/// `visited` holds the declared field types being read on the current path:
/// a record that contains itself adds nothing new there, so the scan of
/// finitely many declarations ends without a depth cutoff, while each
/// occurrence elsewhere is read with its own arguments.
fn type_slot_reads_into<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    visited: &mut Vec<TyId<'db>>,
    reads: &mut Vec<TraitInstId<'db>>,
) {
    struct SlotReads<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        reads: &'a mut Vec<TraitInstId<'db>>,
    }
    impl<'db> TyVisitor<'db> for SlotReads<'_, 'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            let db = self.db;
            match const_ty.data(db) {
                ConstTyData::Abstract(expr, _) => {
                    if let ConstExpr::TraitConst(assoc) = expr.data(db) {
                        for (goal, _) in const_use_reads(db, ConstRef::TraitConst(*assoc)) {
                            self.reads.push(slot_goal(db, goal));
                        }
                    }
                }
                ConstTyData::UnEvaluated {
                    body,
                    ty: Some(expected),
                    capture,
                    ..
                } => {
                    let owner = super::BodyOwner::AnonConstBody {
                        body: *body,
                        expected: *expected,
                    };
                    let typed = &super::infer_body(db, owner).1;
                    for (goal, _) in body_slot_uses(db, typed) {
                        self.reads.push(slot_goal(db, capture.specialize(db, goal)));
                    }
                }
                _ => {}
            }
            walk_const_ty(self, const_ty)
        }
    }
    ty.visit_with(&mut SlotReads { db, reads });
    reads.extend(super::const_requirements::type_condition_slot_reads(db, ty));
    // A record's or enum's field types are part of what its type means. They
    // are read as declared, where a constant is still a term, and
    // instantiated with the type's arguments; the arguments are visited too.
    let mut stack = vec![ty];
    while let Some(ty) = stack.pop() {
        let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
        let (_, args) = ty.decompose_ty_app(db);
        stack.extend(args.iter().copied());
        let Some(adt) = ty.adt_def(db) else {
            continue;
        };
        let args = ty.generic_args(db);
        for variant in adt.fields(db) {
            for field in variant.iter_types(db) {
                let declared_ty = *field.skip_binder();
                if visited.contains(&declared_ty) {
                    continue;
                }
                visited.push(declared_ty);
                let declared = field.map(|field| {
                    let mut declared = Vec::new();
                    type_slot_reads_into(db, field, visited, &mut declared);
                    declared
                });
                visited.pop();
                reads.extend(
                    declared
                        .instantiate(db, args)
                        .into_iter()
                        .map(|goal| slot_goal(db, goal)),
                );
            }
        }
    }
}

impl<'db> TyChecker<'db> {
    /// Refuse a type that reads a constant of a slot the enclosing `with`
    /// opened: no stage could give it one meaning (review section 5).
    pub(super) fn refuse_choice_dependent_ty(
        &mut self,
        ty: TyId<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        if frame.is_none() {
            return false;
        }
        let dependent = type_slot_reads(self.db, ty)
            .into_iter()
            .any(|slot| self.env.choice_frames.lookup(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
    }

    /// The same refusal for the constant bodies written in a type, which
    /// lowering evaluates with the ordinary implementation.
    pub(super) fn refuse_choice_dependent_hir_ty(
        &mut self,
        hir_ty: crate::hir_def::TypeId<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        if frame.is_none() {
            return false;
        }
        let dependent = crate::analysis::ty::ty_lower::hir_ty_const_slot_reads(
            self.db,
            hir_ty,
            self.env.scope(),
            self.env.assumptions(),
        )
        .into_iter()
        .any(|slot| self.env.choice_frames.lookup(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
    }

    /// A call whose instantiated signature reads an opened slot.
    pub(super) fn refuse_choice_dependent_signature(
        &mut self,
        callable: &Callable<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        if self.env.choice_frames.current().is_none() {
            return false;
        }
        let db = self.db;
        let arity = callable.callable_def().arg_tys(db).len();
        let tys = (0..arity)
            .filter_map(|idx| callable.arg_ty(db, idx))
            .chain(std::iter::once(callable.ret_ty(db)))
            .collect::<Vec<_>>();
        tys.into_iter()
            .any(|ty| self.refuse_choice_dependent_ty(ty, primary.clone()))
    }

    /// A type with inference resolved so far, normalized in this body.
    fn resolved_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let ty = self.table.fold_ty(self.db, ty);
        self.normalize_ty(ty)
    }

    /// `with (N)` names an implementation when `N` is not an ordinary value.
    fn named_choice(&mut self, value: ExprId) -> Option<Result<KnownChoice<'db>, &'static str>> {
        let Partial::Present(Expr::Path(Partial::Present(path))) = value.data(self.db, self.body())
        else {
            return None;
        };
        let name = path.as_ident(self.db)?;
        let minter = LoweringContext::new();
        let candidates = named_impl_candidates(self.db, self.env.scope(), name);
        // A name that is both a value and a named implementation is an error
        // that asks which one is meant (RD85); a value alone is a value.
        if !matches!(
            resolve_ident_expr(
                self.db,
                &self.env,
                *path,
                value.span(self.body()).into(),
                &minter
            ),
            ResolvedPathInBody::NewBinding(_)
        ) {
            return (!candidates.is_empty()).then_some(Err(
                "this name is both a value and a named implementation; write `{ name }` for the value, or rename one of them",
            ));
        }
        let [implementation] = candidates.as_slice() else {
            return (!candidates.is_empty()).then_some(Err("the implementation name is ambiguous"));
        };
        let Some(candidate) = lower_impl_trait(self.db, *implementation) else {
            return Some(Err("the named implementation declaration is invalid"));
        };
        if !candidate.params(self.db).is_empty() {
            return Some(Err(
                "a generic named implementation cannot be opened by name",
            ));
        }
        Some(
            KnownChoice::prove(
                self.db,
                self.env.proof_context(),
                *implementation,
                candidate.trait_inst(self.db),
            )
            .map_err(|_| "the named implementation's conditions do not hold here"),
        )
    }

    /// Open an unkeyed `with` binding that names an implementation. `None`
    /// when the binding is not an implementation name; otherwise whether the
    /// opening holds.
    pub(super) fn open_named_binding(
        &mut self,
        header: Option<FrameId>,
        value: ExprId,
    ) -> Option<bool> {
        let named = self.named_choice(value)?;
        let opened = match named {
            Ok(choice) => {
                self.add_premise_if_needed(choice.goal());
                self.push_choice(header, value, choice.goal(), Choice::Known(choice))
            }
            Err(reason) => {
                self.push_diag(BodyDiag::ObligationOpening {
                    primary: value.span(self.body()).into(),
                    reason,
                });
                false
            }
        };
        self.env
            .type_expr(value, ExprProp::new(TyId::unit(self.db), false));
        self.env.with_choice_bindings.push(value);
        Some(opened)
    }

    /// Open an unkeyed `with` binding whose checked value is evidence. `None`
    /// for an effect value; otherwise whether the opening holds.
    pub(super) fn open_evidence_binding(
        &mut self,
        header: Option<FrameId>,
        value: ExprId,
        prop: &ExprProp<'db>,
    ) -> Option<bool> {
        let ty = self.resolved_ty(prop.ty);
        let Some(goal) = evidence_goal(self.db, self.env.scope(), ty) else {
            // Evidence behind a borrow or inside an aggregate is not an
            // effect value; it is refused, not ignored (L2).
            if !self.holds_evidence(ty) {
                return None;
            }
            // A mutable place holding evidence was refused where it was bound.
            let refused_place = prop
                .binding
                .is_some_and(|binding| self.is_mutable_evidence_place(binding.is_mut(), ty));
            if refused_place {
                self.env.with_choice_bindings.push(value);
                return Some(false);
            }
            self.push_diag(BodyDiag::ObligationOpening {
                primary: value.span(self.body()).into(),
                reason: "evidence can be opened only by value, not through a borrow or an aggregate",
            });
            self.env.with_choice_bindings.push(value);
            return Some(false);
        };
        let opened = match self.denote_evidence(value, prop) {
            Ok(choice) => {
                self.add_premise_if_needed(goal);
                self.push_choice(header, value, goal, choice)
            }
            Err(error) => {
                self.report_source_error(value.span(self.body()).into(), error);
                false
            }
        };
        self.env.with_choice_bindings.push(value);
        Some(opened)
    }

    /// The opened goal is a premise of the block when ordinary selection
    /// cannot prove it: a generic goal, or one with only named implementations.
    /// The evidence or the named implementation is its proof.
    fn add_premise_if_needed(&mut self, goal: TraitInstId<'db>) {
        let generic = goal.args(self.db).iter().any(|ty| ty.has_param(self.db));
        if generic
            || !matches!(
                resolve_trait_impl_instance(self.db, self.env.proof_context(), goal),
                Selection::Unique(_)
            )
        {
            self.env.add_opened_premise(goal);
        }
    }

    fn push_choice(
        &mut self,
        header: Option<FrameId>,
        value: ExprId,
        goal: TraitInstId<'db>,
        choice: Choice<'db>,
    ) -> bool {
        let slot = self.env.normalized_slot(goal);
        if self.env.choice_frames.push(header, slot, choice).is_err() {
            self.push_diag(BodyDiag::ObligationOpening {
                primary: value.span(self.body()).into(),
                reason: "one `with` header gives the same trait slot twice",
            });
            return false;
        }
        true
    }

    pub(super) fn report_source_error(&mut self, primary: DynLazySpan<'db>, error: SourceError) {
        self.push_diag(match error {
            SourceError::Runtime => BodyDiag::EvidenceChoiceNotCompileTime { primary },
            SourceError::Unsupported(reason) => BodyDiag::ObligationOpening { primary, reason },
            SourceError::Reported => return,
        });
    }

    /// What an evidence expression means, given its checked property. This is
    /// the only reader of evidence-source syntax: every place that needs a
    /// choice from an evidence value calls it, and `Err` is the only
    /// admission failure.
    pub(super) fn denote_evidence(
        &mut self,
        expr: ExprId,
        prop: &ExprProp<'db>,
    ) -> Result<Choice<'db>, SourceError> {
        if let Some(binding) = prop.binding {
            return match binding {
                // Refused at the parameter (`refuse_evidence_params`).
                LocalBinding::Param { is_mut: true, .. }
                | LocalBinding::Param {
                    site: ParamSite::ContractInit(_),
                    ..
                } => Err(SourceError::Reported),
                LocalBinding::Param { idx, .. } => {
                    Ok(Choice::Slot(SlotRef::EvidenceParam(idx as u16)))
                }
                LocalBinding::Local { pat, .. } => self
                    .env
                    .binding_choice(pat)
                    .unwrap_or(Err(SourceError::Runtime)),
                _ => Err(SourceError::Runtime),
            };
        }
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Path(..)) => match self.env.expr_const_ref(expr) {
                Some(cref @ ConstRef::Const(_)) => {
                    let ty = self.resolved_ty(prop.ty);
                    let value =
                        resolve_semantic_const_ref(self.db, cref, ty, SemOrigin::Expr(expr))
                            .map(|cref| eval_const_ref(self.db, cref));
                    evidence_choice(self.db, value)
                }
                _ => Err(SourceError::Unsupported(
                    "this path does not name a compile-time evidence value",
                )),
            },
            Partial::Present(Expr::Call(_, args)) => {
                if let Some(choice) = self.env.prove_choice(expr) {
                    return Ok(choice);
                }
                let Some(callable) = self.env.semantic_call(expr).cloned() else {
                    return Err(SourceError::Runtime);
                };
                self.factory_choice(&callable, args)
            }
            // A block means its tail; a `with` block means its tail under
            // the block's frame, which the tail's own decision already read.
            Partial::Present(Expr::With(_, body)) => self.denote_tail(*body),
            Partial::Present(Expr::Block(stmts, _)) => match stmts
                .last()
                .map(|stmt| stmt.data(self.db, self.body()))
            {
                Some(Partial::Present(crate::hir_def::Stmt::Expr(tail))) => self.denote_tail(*tail),
                _ => Err(SourceError::Unsupported(
                    "this block does not end in evidence",
                )),
            },
            // A branch chooses evidence by a value (RD121).
            Partial::Present(Expr::If(..) | Expr::Match(..)) => Err(SourceError::Runtime),
            Partial::Present(Expr::Field(base, _)) => {
                // A field of a refused mutable holder was reported there.
                if let Some(LocalBinding::Local { pat, .. }) =
                    self.env.typed_expr(*base).and_then(|prop| prop.binding)
                    && self.env.binding_choice(pat) == Some(Err(SourceError::Reported))
                {
                    return Err(SourceError::Reported);
                }
                Err(SourceError::Unsupported(
                    "evidence stored in an aggregate is not supported",
                ))
            }
            _ => Err(SourceError::Runtime),
        }
    }

    fn denote_tail(&mut self, tail: ExprId) -> Result<Choice<'db>, SourceError> {
        let prop = self.env.typed_expr(tail).ok_or(SourceError::Runtime)?;
        self.denote_evidence(tail, &prop)
    }

    /// A const function call whose arguments are all closed compile-time
    /// values is evaluated now; its result names the choice.
    fn factory_choice(
        &mut self,
        callable: &Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
    ) -> Result<Choice<'db>, SourceError> {
        let (key, values) = self.closed_call(callable, args)?;
        evidence_choice(self.db, Some(eval_instance_with_args(self.db, key, values)))
    }

    /// A closed const call evaluated while checking: the instance the same
    /// call gets anywhere (`checked_call_key`), with the decisions it records
    /// here, and its closed argument values. One meaning for one call.
    fn closed_call(
        &mut self,
        callable: &Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
    ) -> Result<(SemanticInstanceKey<'db>, Vec<SemConstId<'db>>), SourceError> {
        let CallableDef::Func(func) = callable.callable_def() else {
            return Err(SourceError::Runtime);
        };
        if !func.is_const(self.db) {
            return Err(SourceError::Runtime);
        }
        let mut callable = TyFoldable::fold_with(callable.clone(), self.db, &mut self.table);
        if callable
            .generic_args()
            .iter()
            .any(|ty| ty.has_param(self.db) || ty.has_var(self.db))
        {
            return Err(SourceError::Unsupported(
                "an evidence factory with generic arguments is not supported",
            ));
        }
        let values = args
            .iter()
            .map(|arg| self.closed_value(arg.expr))
            .collect::<Result<Vec<_>, _>>()?;
        let unknown = SourceError::Unsupported(
            "a compile-time call that reads a choice not known here is not supported",
        );
        let frame = self.env.choice_frames.current();
        let (own, decided) = self
            .env
            .call_decisions(frame, &callable)
            .map_err(|_| unknown)?;
        if matches!(own, Some(Choice::Slot(_))) {
            return Err(unknown);
        }
        callable.set_choice(own);
        let mut delivered = values
            .iter()
            .enumerate()
            .filter_map(|(idx, value)| match value.value(self.db) {
                SemConstValue::Evidence { choice, .. } => {
                    Some((Delivery::Param(idx as u16), Choice::Known(*choice)))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        delivered.extend(decided);
        callable.set_delivered(delivered);
        let (scope, assumptions) = (self.env.scope(), self.env.assumptions());
        let Some((body, _)) = callable.called_body(self.db, scope, assumptions) else {
            return Err(unknown);
        };
        if body_deliveries(self.db, callable.delivered(), func, body)
            .iter()
            .any(|(_, choice)| matches!(choice, Choice::Slot(_)))
        {
            return Err(unknown);
        }
        let key = checked_call_key(self.db, scope, assumptions, &callable).ok_or(unknown)?;
        Ok((key, values))
    }

    /// The closed argument forms of a factory call: a literal, a const item,
    /// evidence with a known choice, a closed factory-style const call, or an
    /// integer expression the base evaluator computes. `Err` names why an
    /// argument is not closed: a runtime input, or a compile-time form outside
    /// the core.
    fn closed_value(&mut self, expr: ExprId) -> Result<SemConstId<'db>, SourceError> {
        let prop = self.env.typed_expr(expr).ok_or(SourceError::Runtime)?;
        let ty = self.resolved_ty(prop.ty);
        if evidence_goal(self.db, self.env.scope(), ty).is_some() {
            return match self.denote_evidence(expr, &prop)? {
                Choice::Known(choice) => Ok(SemConstId::new(
                    self.db,
                    SemConstValue::Evidence { ty, choice },
                )),
                Choice::Slot(_) => Err(SourceError::Unsupported(
                    "an evidence factory given evidence not known here is not supported",
                )),
            };
        }
        if prop.binding.is_some() {
            return Err(SourceError::Runtime);
        }
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Lit(LitKind::Bool(value))) => {
                return Ok(bool_const(self.db, *value));
            }
            Partial::Present(Expr::Lit(LitKind::Int(value))) => {
                return Ok(int_const(
                    self.db,
                    ty,
                    BigInt::from(value.data(self.db).clone()),
                ));
            }
            Partial::Present(Expr::Path(..)) => {
                let cref = match self.env.expr_const_ref(expr) {
                    Some(cref @ ConstRef::Const(_)) => cref,
                    // A trait constant is read through its decisions here,
                    // as anywhere; one that waits for a choice is not closed.
                    Some(ConstRef::TraitConst(assoc)) => {
                        let frame = self.env.choice_frames.current();
                        let Ok(decided) = self.env.trait_const_decisions(frame, assoc) else {
                            return Err(SourceError::Unsupported(
                                "a factory argument that reads a choice not known here is not supported",
                            ));
                        };
                        if const_use_reads(self.db, ConstRef::TraitConst(decided))
                            .iter()
                            .any(|(_, choice)| matches!(choice, Some(Choice::Slot(_))))
                        {
                            return Err(SourceError::Unsupported(
                                "a factory argument that reads a choice not known here is not supported",
                            ));
                        }
                        ConstRef::TraitConst(decided)
                    }
                    _ => {
                        return Err(SourceError::Unsupported(
                            "a factory argument must be a literal, a const item, evidence, or a closed constant call or integer expression",
                        ));
                    }
                };
                return resolve_semantic_const_ref(self.db, cref, ty, SemOrigin::Expr(expr))
                    .and_then(|cref| eval_const_ref(self.db, cref).into_ready())
                    .ok_or(SourceError::Unsupported(
                        "the constant argument could not be evaluated at compile time",
                    ));
            }
            Partial::Present(Expr::Call(_, args)) => {
                if let Some(callable) = self.env.semantic_call(expr).cloned()
                    && let CallableDef::Func(func) = callable.callable_def()
                    && func.is_const(self.db)
                {
                    let (key, values) = self.closed_call(&callable, args)?;
                    return eval_instance_with_args(self.db, key, values)
                        .into_ready()
                        .ok_or(SourceError::Unsupported(
                            "the constant argument could not be evaluated at compile time",
                        ));
                }
                return Err(SourceError::Runtime);
            }
            _ => {}
        }
        if self.reads_only_literals_and_consts(expr)
            && let Some(value) = crate::analysis::ty::const_ty::try_eval_const_int_expr(
                self.db,
                self.body(),
                expr,
                ty,
            )
        {
            return Ok(int_const(self.db, ty, value));
        }
        Err(SourceError::Unsupported(
            "a factory argument must be a literal, a const item, evidence, or a closed constant call or integer expression",
        ))
    }

    /// A trait or inherent constant in a pattern, with its decisions. A
    /// pattern is fixed while checking, so the choices it reads, its own and
    /// those its constant body receives, must be known now: `None` when one
    /// is pending evidence or the use is refused, never read as the ordinary
    /// implementation (L1).
    pub(super) fn pattern_constant(&self, cref: ConstRef<'db>) -> Option<ConstRef<'db>> {
        let frame = self.env.choice_frames.current();
        let (decided, own, delivered) = match cref {
            ConstRef::TraitConst(use_) => {
                let decided = self.env.trait_const_decisions(frame, use_).ok()?;
                (
                    ConstRef::TraitConst(decided),
                    decided.choice(),
                    decided.delivered(),
                )
            }
            ConstRef::InherentConst(use_) => {
                let decided = self.env.inherent_const_decisions(frame, use_).ok()?;
                (ConstRef::InherentConst(decided), None, decided.delivered())
            }
            ConstRef::Const(_) => return Some(cref),
        };
        let pending = own.is_some_and(|c| c.evidence_param().is_some())
            || delivered.is_some_and(|list| {
                list.items(self.db)
                    .iter()
                    .any(|(_, c)| c.evidence_param().is_some())
            });
        (!pending).then_some(decided)
    }

    /// Whether `expr` is built from literals and const items only, so the base
    /// integer evaluator, which resolves paths from source, reads no trait
    /// choice (a trait constant, a call) in it.
    pub(super) fn reads_only_literals_and_consts(&self, expr: ExprId) -> bool {
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Lit(_)) => true,
            Partial::Present(Expr::Path(..)) => {
                matches!(self.env.expr_const_ref(expr), Some(ConstRef::Const(_)))
            }
            Partial::Present(Expr::Bin(lhs, rhs, _)) => {
                self.reads_only_literals_and_consts(*lhs)
                    && self.reads_only_literals_and_consts(*rhs)
            }
            Partial::Present(Expr::Un(inner, _)) => self.reads_only_literals_and_consts(*inner),
            _ => false,
        }
    }

    /// Record what an immutable `let` binding of an evidence value means, so
    /// later uses of the binding read it instead of re-deriving it.
    pub(super) fn record_binding_choice(&mut self, pat: PatId, init: ExprId, prop: &ExprProp<'db>) {
        let Some(LocalBinding::Local { pat, is_mut }) = self.env.pat_binding(pat) else {
            return;
        };
        let ty = self.resolved_ty(prop.ty);
        if self.is_mutable_evidence_place(is_mut, ty) {
            // No mutable place holds evidence, in a field or not (RD121).
            self.push_diag(BodyDiag::EvidenceMutableStorage {
                primary: pat.span(self.body()).into(),
            });
            self.env.set_binding_choice(pat, Err(SourceError::Reported));
            return;
        }
        if evidence_goal(self.db, self.env.scope(), ty).is_none() {
            return;
        }
        let choice = if is_mut {
            Err(SourceError::Reported)
        } else {
            self.denote_evidence(init, prop)
        };
        self.env.set_binding_choice(pat, choice);
    }

    /// Parameters that cannot hold evidence: a mutable one, since no mutable
    /// place holds evidence (RD121), and one of a contract's `init`, which
    /// has no compile-time caller to choose it (RD116).
    pub(super) fn refuse_evidence_params(&mut self) {
        let params = self.env.params().to_vec();
        for binding in params {
            let LocalBinding::Param {
                ty, is_mut, site, ..
            } = binding
            else {
                continue;
            };
            if !self.holds_evidence(ty) {
                continue;
            }
            let primary = binding.def_span(&self.env);
            if matches!(site, ParamSite::ContractInit(_)) {
                self.push_diag(BodyDiag::EvidenceChoiceNotCompileTime { primary });
            } else if self.is_mutable_evidence_place(is_mut, ty) {
                self.push_diag(BodyDiag::EvidenceMutableStorage { primary });
            }
        }
    }

    /// Whether a value of type `ty` holds evidence, itself or in a field,
    /// element or borrow, at any depth. A record's fields are read as
    /// declared, with its arguments read too, so a record that contains a
    /// growing instance of itself is read in finitely many steps.
    fn holds_evidence(&self, ty: TyId<'db>) -> bool {
        let db = self.db;
        let mut seen = Vec::new();
        let mut pending = vec![ty];
        while let Some(ty) = pending.pop() {
            if seen.contains(&ty) {
                continue;
            }
            seen.push(ty);
            if evidence_goal(db, self.env.scope(), ty).is_some() {
                return true;
            }
            if let Some((_, inner)) = ty.as_capability(db) {
                pending.push(inner);
                continue;
            }
            let (_, args) = ty.decompose_ty_app(db);
            pending.extend(args.iter().copied());
            if let Some(adt) = ty.adt_def(db) {
                for variant in adt.fields(db) {
                    pending.extend(variant.iter_types(db).map(|field| *field.skip_binder()));
                }
            }
        }
        false
    }

    /// The one rule for mutable evidence places (RD121): a mutable binding
    /// of a value that holds evidence, or a mutable borrow of one.
    fn is_mutable_evidence_place(&self, is_mut: bool, ty: TyId<'db>) -> bool {
        (is_mut && self.holds_evidence(ty))
            || matches!(
                ty.as_capability(self.db),
                Some((crate::analysis::ty::ty_def::CapabilityKind::Mut, inner))
                    if self.holds_evidence(inner)
            )
    }

    /// Denote the evidence arguments of a call, in parameter order.
    pub(super) fn deliver_evidence_args(
        &mut self,
        callable: &mut Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
        first_param: usize,
    ) {
        let mut delivered = Vec::new();
        for (offset, arg) in args.iter().enumerate() {
            let Some(prop) = self.env.typed_expr(arg.expr) else {
                continue;
            };
            let ty = self.resolved_ty(prop.ty);
            if evidence_goal(self.db, self.env.scope(), ty).is_none() {
                continue;
            }
            match self.denote_evidence(arg.expr, &prop) {
                Ok(choice) => {
                    delivered.push((Delivery::Param((first_param + offset) as u16), choice))
                }
                Err(error) => self.report_source_error(arg.expr.span(self.body()).into(), error),
            }
        }
        callable.set_delivered(delivered);
    }

    /// `prove<G>()` answers `G` where it is written: from the frame in force,
    /// otherwise with the ordinary implementation of a concrete `G` (RD103 Q6).
    pub(super) fn decide_prove(&mut self, expr: ExprId, callable: &mut Callable<'db>) -> bool {
        let goal = callable
            .generic_args()
            .first()
            .map(|term| self.resolved_ty(*term))
            .and_then(|term| match term.data(self.db) {
                TyData::ConstraintTerm(goal) => Some(*goal),
                _ => None,
            });
        let result = match goal {
            None => Err("expected one trait goal"),
            Some(goal) => {
                let slot = self.env.normalized_slot(goal);
                match self
                    .env
                    .choice_frames
                    .lookup(self.env.choice_frames.current(), slot)
                {
                    Some(choice) if self.choice_answers(choice, goal) => Ok(choice),
                    Some(_) => Err("the opened implementation does not prove the requested goal"),
                    None if slot.args(self.db).iter().any(|ty| ty.has_param(self.db)) => {
                        Err("a generic goal can be proved only under a `with` that opens it")
                    }
                    None => self.ordinary_choice(goal).map(Choice::Known),
                }
            }
        };
        match result {
            Ok(choice) => {
                callable.set_choice(Some(choice));
                true
            }
            Err(reason) => {
                self.push_diag(BodyDiag::ObligationIntroduction {
                    primary: expr.span(self.body()).into(),
                    reason,
                });
                false
            }
        }
    }

    /// Whether the frame's choice for `goal`'s slot answers all of `goal`. A
    /// pending choice answers its associated-type equalities when they hold
    /// in the block: through the opened goal, a premise there for a slot with
    /// only named implementations, or through the ordinary implementation,
    /// whose associated types every named one shares (RD108).
    fn choice_answers(&self, choice: Choice<'db>, goal: TraitInstId<'db>) -> bool {
        match choice {
            Choice::Known(known) => known.answers(self.db, goal),
            Choice::Slot(_) => {
                goal.assoc_type_bindings(self.db).is_empty()
                    || matches!(
                        is_goal_satisfiable(self.db, self.env.proof_context(), goal),
                        GoalSatisfiability::Satisfied(_)
                    )
            }
        }
    }

    fn ordinary_choice(&self, goal: TraitInstId<'db>) -> Result<KnownChoice<'db>, &'static str> {
        let cx = self.env.proof_context();
        let Selection::Unique(resolved) = resolve_trait_impl_instance(self.db, cx, goal) else {
            return Err("the requested trait goal was not proved");
        };
        let ImplementorOrigin::Hir(implementation) = resolved.selected().origin(self.db) else {
            return Err("the requested trait goal has no authored implementation");
        };
        KnownChoice::prove(self.db, cx, implementation, goal)
            .map_err(|_| "the implementation's conditions were not admitted")
    }
}

fn evidence_choice<'db>(
    db: &'db dyn HirAnalysisDb,
    value: Option<EvalOutcome<'db, SemConstId<'db>>>,
) -> Result<Choice<'db>, SourceError> {
    match value {
        Some(EvalOutcome::Ready(value)) => match value.value(db) {
            SemConstValue::Evidence { choice, .. } => Ok(Choice::Known(*choice)),
            _ => Err(SourceError::Unsupported(
                "the evidence source did not produce an evidence value",
            )),
        },
        Some(EvalOutcome::Failed(_)) => Err(SourceError::Unsupported(
            "the evidence source failed to evaluate at compile time",
        )),
        Some(EvalOutcome::Blocked(_)) | None => Err(SourceError::Unsupported(
            "the evidence source could not be evaluated at compile time",
        )),
    }
}

/// A call's decisions: its own slot's, and what it delivers to the body
/// that runs.
type CallDecisions<'db> = (Option<Choice<'db>>, Vec<(Delivery<'db>, Choice<'db>)>);

impl<'db> TyCheckEnv<'db> {
    /// Whether `frame` answers all of `goal`: inside the block that opened
    /// its slot, the goal holds even with no ordinary implementation, by a
    /// known choice (whose conditions were checked when it was opened, and
    /// only with the associated types it has) or by evidence.
    pub(super) fn frame_proves(&self, frame: Option<FrameId>, goal: TraitInstId<'db>) -> bool {
        let db = self.db();
        match self.choice_frames.lookup(frame, self.normalized_slot(goal)) {
            Some(Choice::Known(known)) => known.answers(db, goal),
            // Evidence for exactly this slot proves it. Its associated types
            // are not known until it arrives, so a goal that states them is
            // left to the ordinary proof.
            Some(Choice::Slot(_)) => goal.assoc_type_bindings(db).is_empty(),
            None => false,
        }
    }

    /// The one decision rule for a use of `inst`'s slot checked in `frame`:
    /// the frame's entry for the slot, otherwise the body's own bound for a
    /// generic slot, otherwise none (ordinary).
    pub(super) fn decide(
        &self,
        frame: Option<FrameId>,
        inst: TraitInstId<'db>,
    ) -> Option<Choice<'db>> {
        let db = self.db();
        let slot = self.normalized_slot(inst);
        if let Some(found) = self.choice_frames.lookup(frame, slot) {
            return Some(found);
        }
        let written = slot_goal(db, inst);
        if let Some(own) = self
            .base_assumptions()
            .list(db)
            .iter()
            .map(|bound| slot_goal(db, *bound))
            .find(|own| {
                own.args(db).iter().any(|ty| ty.has_param(db)) && (*own == written || *own == slot)
            })
        {
            return Some(Choice::Slot(SlotRef::Bound(own)));
        }
        None
    }

    /// `inst`'s slot with its arguments normalized in this body, so that a
    /// projection that names a concrete type is read as that type.
    fn normalized_slot(&self, inst: TraitInstId<'db>) -> TraitInstId<'db> {
        let db = self.db();
        let slot = slot_goal(db, inst);
        let args = slot
            .args(db)
            .iter()
            .map(|ty| {
                crate::analysis::ty::normalize::normalize_ty(
                    db,
                    *ty,
                    self.scope(),
                    self.assumptions(),
                )
            })
            .collect::<Vec<_>>();
        if args == *slot.args(db) {
            return slot;
        }
        TraitInstId::new(db, slot.def(db), args, common::indexmap::IndexMap::new())
    }

    /// The decisions a direct call checked in `frame` records: its trait
    /// slot's, and one for each bound of the body that runs, decided for the
    /// goal it means where the call is written. A body known here is
    /// delivered by its own bound names, a body chosen later by the callee
    /// declaration's (`trait_choice::body_deliveries`, `role_map`). `Err`
    /// when a goal could coincide with an opened slot without being it, or
    /// for a conditional implementation (`ChoiceRefusal`).
    pub(super) fn call_decisions(
        &self,
        frame: Option<FrameId>,
        callable: &Callable<'db>,
    ) -> Result<CallDecisions<'db>, ChoiceRefusal> {
        let db = self.db();
        let own = callable
            .trait_inst
            .and_then(|inst| self.decide(frame, inst));
        let generic = self
            .base_assumptions()
            .list(db)
            .iter()
            .any(|bound| bound.args(db).iter().any(|ty| ty.has_param(db)));
        let CallableDef::Func(func) = callable.callable_def() else {
            return Ok((own, Vec::new()));
        };
        if frame.is_none() && !generic {
            return Ok((own, Vec::new()));
        }
        let mut called = callable.clone();
        called.set_choice(own);
        let known = called.called_body(db, self.scope(), self.assumptions());
        let (body, body_args, by_name) = match &known {
            Some((body, body_args)) => (*body, body_args.as_slice(), true),
            None => (func, callable.generic_args(), false),
        };
        let mut decided = Vec::new();
        for (bound, at_call) in declared_goals(db, body, body_args) {
            decided.push((bound, at_call, self.decide(frame, at_call)));
        }
        // A body chosen later is one of the trait's implementations of the
        // goal. Each candidate that applies to the goal as written has its own
        // bounds decided here, so whichever runs receives what it declares, as
        // it would if it were known here (L4). A candidate that applies only
        // for some generic arguments has no bound written here, so the frame
        // does not reach it (the written reading).
        let mut candidate_deliveries = Vec::new();
        let mut undecided_candidates = Vec::new();
        if known.is_none() && (frame.is_some() || generic) {
            let candidates = self.candidate_bodies(callable, func);
            for (candidate, candidate_args) in candidates {
                for (bound, at_call) in declared_goals(db, candidate, &candidate_args) {
                    if role_map(db, func, candidate)
                        .iter()
                        .any(|(_, in_body)| *in_body == bound)
                    {
                        continue;
                    }
                    match self.decide(frame, at_call) {
                        Some(choice) => candidate_deliveries.push((Delivery::Bound(bound), choice)),
                        None => undecided_candidates.push(at_call),
                    }
                }
            }
        }
        // A callee given ordinary evidence for a bound selects that bound's
        // implementation itself, where an opened slot its conditions read is
        // not in force. The call's own slot is exempt: the body that runs
        // for it receives its implementation's bounds here.
        let own_slot = callable.trait_inst.map(|inst| slot_goal(db, inst));
        let undecided = decided
            .iter()
            .filter(|(_, _, choice)| choice.is_none())
            .map(|(_, goal, _)| *goal)
            .chain(undecided_candidates);
        if self.reads_opened_condition(frame, undecided, own_slot) {
            return Err(ChoiceRefusal::OpenedCondition);
        }
        let mut delivered: Vec<(Delivery<'db>, Choice<'db>)> = Vec::new();
        for (bound, _, choice) in decided {
            let Some(choice) = choice else {
                continue;
            };
            let delivery = if by_name {
                Delivery::Bound(bound)
            } else {
                Delivery::Nominal(bound)
            };
            if !delivered.contains(&(delivery, choice)) {
                delivered.push((delivery, choice));
            }
        }
        for item in candidate_deliveries {
            if !delivered.contains(&item) {
                delivered.push(item);
            }
        }
        Ok((own, delivered))
    }

    /// The implementations, ordinary and named, that apply to `inst` as
    /// written (normalized here), each applied to it: the bodies a use whose
    /// implementation is chosen later could run. None for a goal that still
    /// holds inference variables.
    fn applicable_impls(
        &self,
        inst: TraitInstId<'db>,
    ) -> Vec<crate::analysis::ty::trait_def::ResolvedImplInstance<'db>> {
        use crate::analysis::ty::{
            trait_def::{impls_for_trait_def, instantiate_selected_impl, named_impls_for_trait},
            trait_lower::complete_selected_impl,
        };
        let db = self.db();
        let inst = self.normalized_slot(inst);
        if inst.args(db).iter().any(|ty| ty.has_var(db)) {
            return Vec::new();
        }
        let ingot = self.scope().top_mod(db).ingot(db);
        let mut candidates = impls_for_trait_def(db, ingot, inst.def(db)).clone();
        candidates.extend(
            named_impls_for_trait(db, ingot, inst.def(db))
                .into_iter()
                .filter_map(|item| lower_impl_trait(db, item)),
        );
        candidates
            .into_iter()
            .filter(|candidate| matches!(candidate.origin(db), ImplementorOrigin::Hir(_)))
            .filter_map(|candidate| {
                complete_selected_impl(db, candidate)
                    .and_then(|completed| instantiate_selected_impl(db, completed, inst))
            })
            .collect()
    }

    /// The method bodies a trait call whose implementation is chosen later
    /// could run, with their arguments (`applicable_impls`).
    fn candidate_bodies(
        &self,
        callable: &Callable<'db>,
        nominal: crate::hir_def::Func<'db>,
    ) -> Vec<(crate::hir_def::Func<'db>, Vec<TyId<'db>>)> {
        use crate::analysis::ty::trait_def::method_instance_from_resolved;
        let db = self.db();
        let (Some(inst), Some(name)) = (callable.trait_inst, nominal.name(db).to_opt()) else {
            return Vec::new();
        };
        let mut bodies = Vec::new();
        for resolved in self.applicable_impls(inst) {
            let Selection::Unique(method) = method_instance_from_resolved(db, resolved, name)
            else {
                continue;
            };
            let Some(body) = method.body() else {
                continue;
            };
            let Ok(args) = method.complete_body_args(db, callable.generic_args()) else {
                continue;
            };
            let entry = (body, args.into_values());
            if !bodies.contains(&entry) {
                bodies.push(entry);
            }
        }
        bodies
    }

    /// Whether a bound left to its callee's ordinary selection, other than
    /// `exempt` (a use's own slot), is proved through a condition on a slot
    /// `frame` opened (D13).
    fn reads_opened_condition(
        &self,
        frame: Option<FrameId>,
        undecided: impl IntoIterator<Item = TraitInstId<'db>>,
        exempt: Option<TraitInstId<'db>>,
    ) -> bool {
        frame.is_some()
            && undecided
                .into_iter()
                .any(|goal| Some(goal) != exempt && self.conditions_read_frame(frame, goal))
    }

    /// Whether the ordinary proof of `goal` uses, at any depth, an
    /// implementation condition whose slot `frame` opened.
    fn conditions_read_frame(&self, frame: Option<FrameId>, goal: TraitInstId<'db>) -> bool {
        let db = self.db();
        let mut seen = Vec::new();
        let mut pending = vec![goal];
        while let Some(goal) = pending.pop() {
            if seen.contains(&goal) {
                continue;
            }
            // A proof whose conditions do not end is not known to avoid the
            // frame; it is treated as reading it (unknown is never "holds").
            if seen.len() > 256 {
                return true;
            }
            seen.push(goal);
            let Selection::Unique(resolved) =
                resolve_trait_impl_instance(db, self.proof_context(), goal)
            else {
                continue;
            };
            let ImplementorOrigin::Hir(implementation) = resolved.selected().origin(db) else {
                continue;
            };
            for condition in collect_constraints(
                db,
                crate::hir_def::GenericParamOwner::ImplTrait(implementation),
            )
            .instantiate(db, resolved.impl_args(db))
            .list(db)
            .iter()
            {
                if self
                    .choice_frames
                    .lookup(frame, self.normalized_slot(*condition))
                    .is_some()
                {
                    return true;
                }
                pending.push(*condition);
            }
        }
        false
    }

    /// The slots a use of `callable` reads where it is written: its trait
    /// slot, the goals its callee declares, and, when the body that runs is
    /// known here, the goals that body declares (an implementation's bounds).
    fn call_uses(&self, callable: &Callable<'db>) -> Vec<TraitInstId<'db>> {
        let db = self.db();
        let CallableDef::Func(func) = callable.callable_def() else {
            return Vec::new();
        };
        let mut uses = callable.trait_inst.into_iter().collect::<Vec<_>>();
        uses.extend(
            declared_goals(db, func, callable.generic_args())
                .into_iter()
                .map(|(_, at_call)| at_call),
        );
        if let Some((body, body_args)) = callable.called_body(db, self.scope(), self.assumptions())
            && body != func
        {
            uses.extend(
                declared_goals(db, body, &body_args)
                    .into_iter()
                    .map(|(_, at_call)| at_call),
            );
        }
        uses
    }

    /// Record each trait use's decision, each direct call's deliveries, and
    /// the choices each function value would carry. A use whose slot could
    /// coincide with an opened slot without being it is recorded as
    /// ambiguous and refused. Runs once inference has resolved every goal.
    pub(super) fn record_choice_decisions(&mut self) {
        let db = self.db();
        // A call through a function value made elsewhere receives nothing from
        // the frame where it is called; only a path naming the function does.
        let mut path_callees = Vec::new();
        for (expr, data) in self.body().exprs(db).iter() {
            if let Partial::Present(Expr::Call(callee, _)) = data
                && calls_directly(db, self.body(), expr, |callee| {
                    self.typed_expr(callee)
                        .is_some_and(|prop| prop.binding.is_some())
                })
            {
                path_callees.push(*callee);
            }
        }
        let exprs = self
            .semantic_expr_lowering
            .iter()
            .filter_map(|(expr, lowering)| lowering.is_some().then_some(expr))
            .collect::<Vec<_>>();
        for expr in exprs {
            let frame = self.use_frames[expr];
            let Some(SemanticExprLowering::Call { callable }) =
                self.semantic_expr_lowering[expr].as_ref()
            else {
                continue;
            };
            let mut callable = callable.clone();
            let direct = match expr.data(db, self.body()) {
                Partial::Present(Expr::Call(callee, _)) => path_callees.contains(callee),
                _ => true,
            };
            if direct {
                let (own, decided) = match self.call_decisions(frame, &callable) {
                    Ok(decisions) => decisions,
                    Err(refusal) => {
                        self.choice_refusals.push((expr, refusal));
                        (None, Vec::new())
                    }
                };
                if callable.choice().is_none() {
                    callable.set_choice(own);
                }
                let mut delivered = callable.delivered().to_vec();
                delivered.extend(decided);
                callable.set_delivered(delivered);
            }
            self.semantic_expr_lowering[expr] = Some(SemanticExprLowering::Call { callable });
        }
        // A function value is a path naming a function that no call takes as
        // its callee. It would carry the decisions for the slots it reads.
        let values = self
            .body()
            .exprs(db)
            .keys()
            .filter(|expr| {
                !path_callees.contains(expr)
                    && matches!(expr.data(db, self.body()), Partial::Present(Expr::Path(..)))
            })
            .filter_map(|expr| {
                let prop = self.typed_expr(expr)?;
                if prop.binding.is_some() {
                    return None;
                }
                if let Some(callable) = self.callables[expr].clone() {
                    return Some((expr, callable));
                }
                let callable = Callable::new(db, prop.ty, DynLazySpan::invalid(), None).ok()?;
                Some((expr, callable))
            })
            .collect::<Vec<_>>();
        for (expr, callable) in values {
            let frame = self.use_frames[expr];
            let mut captured = Vec::new();
            for goal in self.call_uses(&callable) {
                if let Some(choice) = self.decide(frame, goal)
                    && !captured.contains(&choice)
                {
                    captured.push(choice);
                }
            }
            if !captured.is_empty() {
                self.function_value_choices.push((expr, captured));
            }
        }
        let exprs = self
            .const_refs
            .iter()
            .filter_map(|(expr, cref)| {
                matches!(
                    cref,
                    Some(ConstRef::TraitConst(_) | ConstRef::InherentConst(_))
                )
                .then_some(expr)
            })
            .collect::<Vec<_>>();
        for expr in exprs {
            let frame = self.use_frames[expr];
            let decided = match self.const_refs[expr] {
                Some(ConstRef::TraitConst(use_)) => self
                    .trait_const_decisions(frame, use_)
                    .map(ConstRef::TraitConst),
                Some(ConstRef::InherentConst(use_)) => self
                    .inherent_const_decisions(frame, use_)
                    .map(ConstRef::InherentConst),
                _ => continue,
            };
            match decided {
                Ok(decided) => self.const_refs[expr] = Some(decided),
                Err(refusal) => self.choice_refusals.push((expr, refusal)),
            }
        }
    }

    /// A trait constant use with its decisions: its own slot's, and one for
    /// each bound of the constant body that runs (an implementation's bounds,
    /// a default body's `Self` and supertraits), as for a call. `Err` as for
    /// a call (`call_decisions`).
    pub(super) fn trait_const_decisions(
        &self,
        frame: Option<FrameId>,
        use_: crate::analysis::ty::assoc_const::AssocConstUse<'db>,
    ) -> Result<crate::analysis::ty::assoc_const::AssocConstUse<'db>, ChoiceRefusal> {
        let db = self.db();
        let own = self.decide(frame, use_.inst());
        let use_ = use_.with_choice(own);
        let mut undecided = Vec::new();
        let items = match use_.body_that_runs(db) {
            Some((body, subst)) => self.body_decisions(
                frame,
                const_declared_goals(db, body, &subst),
                false,
                &mut undecided,
            ),
            // A body chosen later: the trait's default body declares the
            // slots a constant's declaration has (its `Self` and supertraits),
            // reached by name (`DeliveryList::to_body`), and each
            // implementation that applies to the goal as written has its
            // constant's bounds decided here, as a call's candidates are.
            None => {
                let mut items = match use_.trait_default_body(db) {
                    Some((body, subst)) => self.body_decisions(
                        frame,
                        const_declared_goals(db, body, &subst),
                        true,
                        &mut undecided,
                    ),
                    None => Vec::new(),
                };
                for resolved in self.applicable_impls(use_.inst()) {
                    if let Some((body, _, subst)) =
                        crate::analysis::ty::trait_def::selected_assoc_const_body_template(
                            db,
                            resolved,
                            use_.name(),
                        )
                    {
                        for item in self.body_decisions(
                            frame,
                            const_declared_goals(db, body, &subst),
                            false,
                            &mut undecided,
                        ) {
                            if !items.contains(&item) {
                                items.push(item);
                            }
                        }
                    }
                }
                items
            }
        };
        if self.reads_opened_condition(frame, undecided, Some(slot_goal(db, use_.inst()))) {
            return Err(ChoiceRefusal::OpenedCondition);
        }
        Ok(use_.with_delivered((!items.is_empty()).then(|| DeliveryList::new(db, items))))
    }

    /// An inherent constant use with the decisions for its implementation's
    /// bounds.
    pub(super) fn inherent_const_decisions(
        &self,
        frame: Option<FrameId>,
        use_: crate::analysis::ty::assoc_const::InherentConstUse<'db>,
    ) -> Result<crate::analysis::ty::assoc_const::InherentConstUse<'db>, ChoiceRefusal> {
        let db = self.db();
        let Some((body, impl_args)) =
            crate::analysis::ty::const_ty::inherent_const_body_and_impl_args(
                db,
                use_.impl_(),
                use_.receiver_ty(),
                use_.name(),
            )
        else {
            return Ok(use_);
        };
        let Ok(subst) = crate::analysis::ty::ty_lower::CompleteSubst::for_owner(
            db,
            use_.impl_().into(),
            impl_args,
        ) else {
            return Ok(use_);
        };
        let mut undecided = Vec::new();
        let items = self.body_decisions(
            frame,
            const_declared_goals(db, body, &subst),
            false,
            &mut undecided,
        );
        if self.reads_opened_condition(frame, undecided, None) {
            return Err(ChoiceRefusal::OpenedCondition);
        }
        Ok(use_.with_delivered((!items.is_empty()).then(|| DeliveryList::new(db, items))))
    }

    /// The decisions for a constant body's bounds, by its own names; the
    /// goals left ordinary are added to `undecided`.
    fn body_decisions(
        &self,
        frame: Option<FrameId>,
        goals: Vec<(TraitInstId<'db>, TraitInstId<'db>)>,
        nominal: bool,
        undecided: &mut Vec<TraitInstId<'db>>,
    ) -> Vec<(Delivery<'db>, Choice<'db>)> {
        let db = self.db();
        let generic = self
            .base_assumptions()
            .list(db)
            .iter()
            .any(|bound| bound.args(db).iter().any(|ty| ty.has_param(db)));
        if frame.is_none() && !generic {
            return Vec::new();
        }
        let mut items = Vec::new();
        for (bound, at_use) in goals {
            let Some(choice) = self.decide(frame, at_use) else {
                undecided.push(at_use);
                continue;
            };
            let delivery = if nominal {
                Delivery::Nominal(bound)
            } else {
                Delivery::Bound(bound)
            };
            items.push((delivery, choice));
        }
        items
    }
}

/// The bound slots of `func` whose choices reach a type `func` itself
/// writes or names: in its body, in its signature (marked), and in a function
/// value that would carry the slot's choice. A caller may not deliver an
/// opened choice into them (`OwedCheck::Reaches`); slots reached through
/// callees are added by `owed_checks`.
pub(super) fn own_type_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    typed: &super::TypedBody<'db>,
) -> Vec<(TraitInstId<'db>, bool)> {
    let own = own_slots(db, func);
    if own.is_empty() {
        return Vec::new();
    }
    let mut reached: Vec<(TraitInstId<'db>, bool)> = Vec::new();
    let mut note = |goal: TraitInstId<'db>, signature: bool| {
        if own.contains(&goal) && !reached.contains(&(goal, signature)) {
            reached.push((goal, signature));
        }
    };
    for goal in signature_slot_reads(db, func) {
        note(goal, true);
    }
    let Some(body) = typed.body() else {
        return reached;
    };
    for expr in body.exprs(db).keys() {
        for goal in type_slot_reads(db, typed.expr_ty(db, expr)) {
            note(goal, false);
        }
    }
    for pat in body.pats(db).keys() {
        for goal in type_slot_reads(db, typed.pat_ty(db, pat)) {
            note(goal, false);
        }
    }
    for (_, captured) in typed.function_value_choices() {
        for choice in captured {
            if let Choice::Slot(SlotRef::Bound(goal)) = choice {
                note(*goal, false);
            }
        }
    }
    reached
}

fn signature_slot_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<TraitInstId<'db>> {
    let callable = CallableDef::Func(func);
    callable
        .arg_tys(db)
        .iter()
        .map(|ty| *ty.skip_binder())
        .chain(std::iter::once(*callable.ret_ty(db).skip_binder()))
        .flat_map(|ty| type_slot_reads(db, ty))
        .collect()
}

/// The refusals a checked body's choice uses need once inference is done:
/// a function value that would carry an opened choice, a call that delivers
/// an opened or evidence choice into a slot reaching a type of a body it
/// runs, and a use whose slot could coincide with an opened slot without
/// being it.
pub(super) fn check_choice_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &super::TypedBody<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
) -> Vec<crate::analysis::ty::diagnostics::FuncBodyDiag<'db>> {
    let Some(body) = typed.body() else {
        return Vec::new();
    };
    let mut diags = Vec::new();
    for (expr, refusal) in typed.choice_refusals() {
        let reason = match refusal {
            ChoiceRefusal::OpenedCondition => {
                "a bound of this call is answered by an implementation with a condition on a slot opened here; conditional implementations under `with` are not supported yet"
            }
        };
        diags.push(
            BodyDiag::ObligationOpening {
                primary: expr.span(body).into(),
                reason,
            }
            .into(),
        );
    }
    for (expr, captured) in typed.function_value_choices() {
        if captured
            .iter()
            .any(|choice| !matches!(choice, Choice::Slot(SlotRef::Bound(_))))
        {
            diags.push(
                BodyDiag::ChoiceDependentType {
                    primary: expr.span(body).into(),
                }
                .into(),
            );
        }
    }
    for expr in body.exprs(db).keys() {
        if typed.callable_expr(expr).is_none() {
            continue;
        }
        use super::const_requirements::{EdgeRefusal, call_refusal};
        let primary = expr.span(body).into();
        match call_refusal(db, typed, body, scope, expr) {
            Some(EdgeRefusal::TypeReach) => {
                diags.push(BodyDiag::ChoiceDependentType { primary }.into())
            }
            Some(EdgeRefusal::TooDeep) => diags.push(
                BodyDiag::ObligationOpening {
                    primary,
                    reason: "the checks this call owes through bodies chosen later could not be followed to an end",
                }
                .into(),
            ),
            None => {}
        }
    }
    diags
}
