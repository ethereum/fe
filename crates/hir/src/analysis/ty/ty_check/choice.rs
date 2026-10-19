//! The checker's side of trait choices.
//!
//! `with` maps slots to choices for its body. [`TyChecker::denote_evidence`]
//! is the only function that gives an evidence expression its meaning, so it
//! is also the only place that admits or refuses an evidence source. Each
//! trait use records one decision when the body is finished, from the frame it
//! was checked in.

use num_bigint::BigInt;

use super::{
    Callable, ConstRef, ExprProp, ForLoopPlan, LocalBinding, SemanticExprLowering, TyChecker,
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
            corelib::{evidence_goal, lib_func_matches},
            diagnostics::BodyDiag,
            fold::{TyFoldable, TyFolder},
            trait_choice::{
                Choice, ChoiceRefusal, Delivery, DeliveryList, KnownChoice, SlotRef,
                body_deliveries, const_declared_goals, declared_goals, impl_declared_goals,
                own_slots, role_map, slot_goal,
            },
            trait_def::{ImplementorOrigin, TraitInstId, resolve_trait_impl_instance},
            trait_lower::lower_impl_trait,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, Selection, TraitSolveCx,
                constraint::collect_constraints, is_goal_satisfiable,
            },
            ty_def::{TyData, TyId},
            visitor::{TyVisitable, TyVisitor, walk_const_ty},
        },
    },
    hir_def::{BinOp, CallableDef, Expr, ExprId, LitKind, Partial, PatId, StmtId},
    span::DynLazySpan,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct FrameId(u32);

/// Whether any frame is in force at `frame`, the frame a use is checked in.
/// Every shortcut that skips choice work where nothing is opened reads it,
/// so the shortcuts move together: with a module-level frame as the base of
/// the frame list, "no frame" stops meaning "nothing is opened" here only.
pub(super) fn has_frames(frame: Option<FrameId>) -> bool {
    frame.is_some()
}

/// Whether a value of type `ty` holds evidence, itself or in a field,
/// element or borrow, at any depth. A record's fields are read as declared,
/// with its arguments read too, so a record that contains a growing instance
/// of itself is read in finitely many steps.
pub(super) fn holds_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    ty: TyId<'db>,
) -> bool {
    let mut seen = Vec::new();
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        if seen.contains(&ty) {
            continue;
        }
        seen.push(ty);
        if evidence_goal(db, scope, ty).is_some() {
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

/// The type parameter a value of type `ty` is, behind any capability.
pub(super) fn as_type_param<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
    matches!(ty.data(db), TyData::TyParam(_)).then_some(ty)
}

/// Evidence in a field, an element or a destructured part of an
/// aggregate has no admitted source.
const AGGREGATE: &str = "evidence stored in an aggregate is not supported";

/// How many steps a walk that follows trait choices through implementations
/// takes before it stops, a backstop for the walk itself. The bound that
/// matters is the instance limits: without growing arguments a walk comes
/// back to a step it has already taken.
const CHOICE_WALK_BACKSTOP: usize = 4096;

/// A step of a walk that follows trait choices through implementations.
pub(super) enum WalkStep<'a, 'db> {
    /// Into a body the call runs, with these arguments: it becomes an
    /// instance, so the base's instance limits apply (the 16-0016 test).
    Instance(&'a [TyId<'db>]),
    /// To a condition of a proof the base has already found: the proof is
    /// finite, and only the walk itself needs a backstop. The base sets no
    /// limit on constants read through implementations.
    Proof,
}

/// Whether a walk that follows trait choices through implementations (the
/// conditions of a proof under a `with`, the checks a call owes through
/// bodies chosen later) may take `step` after `steps` steps. Every such walk
/// ends here: where the base's limits end what it follows, or at the
/// backstop.
pub(super) fn choice_walk_step_within_limits<'db>(
    db: &'db dyn HirAnalysisDb,
    step: WalkStep<'_, 'db>,
    steps: usize,
) -> bool {
    steps < CHOICE_WALK_BACKSTOP
        && match step {
            WalkStep::Instance(args) => {
                crate::analysis::ty::normalize::instance_args_within_limits(db, args)
            }
            WalkStep::Proof => true,
        }
}

/// A body node whose own written paths `refuse_written_const_args` scans.
#[derive(Clone, Copy)]
pub(super) enum WrittenNode {
    Expr(ExprId),
    Pat(PatId),
}

/// The key frames give `inst`'s slot, normalized in `scope`: the one way a
/// key is made, for the checker's frames and for the frames a constant
/// written in a body reads (`written_frames`).
pub(super) fn slot_key<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
) -> SlotKey<'db> {
    SlotKey::new(db, inst, scope, assumptions)
}

mod slot_key {
    use crate::analysis::{
        HirAnalysisDb,
        ty::{
            normalize::normalize_ty, trait_choice::slot_goal, trait_def::TraitInstId,
            trait_resolution::PredicateListId,
        },
    };
    use crate::hir_def::scope_graph::ScopeId;

    /// A slot as frames key it: no associated-type equalities, arguments
    /// normalized in the body, so a projection that names a concrete type is
    /// read as that type. Its only constructor normalizes, so every frame
    /// writer and reader agrees on the key.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(in super::super) struct SlotKey<'db>(TraitInstId<'db>);

    impl<'db> SlotKey<'db> {
        pub(in super::super) fn new(
            db: &'db dyn HirAnalysisDb,
            inst: TraitInstId<'db>,
            scope: ScopeId<'db>,
            assumptions: PredicateListId<'db>,
        ) -> Self {
            let slot = slot_goal(db, inst);
            let args = slot
                .args(db)
                .iter()
                .map(|ty| normalize_ty(db, *ty, scope, assumptions))
                .collect::<Vec<_>>();
            if args == *slot.args(db) {
                return Self(slot);
            }
            Self(TraitInstId::new(
                db,
                slot.def(db),
                args,
                common::indexmap::IndexMap::new(),
                slot.mode(db),
            ))
        }

        pub(in super::super) fn inst(self) -> TraitInstId<'db> {
            self.0
        }
    }
}
pub(super) use slot_key::SlotKey;

mod frames {
    //! Frames are read only through `lookup` (and `opens_trait`, which
    //! reads only the trait of each slot): their nodes are private here.
    use super::{Choice, FrameId, HirAnalysisDb, SlotKey};

    struct FrameNode<'db> {
        parent: Option<FrameId>,
        slot: SlotKey<'db>,
        choice: Choice<'db>,
    }

    /// The lexical frames of choices, kept as a persistent list so that a use can
    /// remember the frame it was checked in and be decided after inference.
    #[derive(Default)]
    pub(in super::super) struct ChoiceFrames<'db> {
        nodes: Vec<FrameNode<'db>>,
        current: Option<FrameId>,
    }

    impl<'db> ChoiceFrames<'db> {
        pub(in super::super) fn current(&self) -> Option<FrameId> {
            self.current
        }

        pub(in super::super) fn restore(&mut self, frame: Option<FrameId>) {
            self.current = frame;
        }

        /// The frame a body starts in: inside `impl T for S as N`, the slot
        /// `T<S>` is N for every use of that slot in the body, `Self` forms,
        /// receiver calls and concrete paths alike; an inner `with`
        /// wins over it.
        pub(super) fn seed(&mut self, slot: SlotKey<'db>, choice: Choice<'db>) {
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
        pub(super) fn push(
            &mut self,
            header: Option<FrameId>,
            slot: SlotKey<'db>,
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

        /// The slots `frame` and the frames around it open: the premises in
        /// force there.
        pub(super) fn opened(&self, frame: Option<FrameId>) -> Vec<super::TraitInstId<'db>> {
            let mut opened = Vec::new();
            let mut at = frame;
            while let Some(id) = at {
                let node = &self.nodes[id.0 as usize];
                opened.push(node.slot.inst());
                at = node.parent;
            }
            opened
        }

        /// Whether `frame` or a frame around it opens a slot of `trait_`.
        pub(super) fn opens_trait(
            &self,
            db: &'db dyn HirAnalysisDb,
            frame: Option<FrameId>,
            trait_: crate::hir_def::Trait<'db>,
        ) -> bool {
            let mut at = frame;
            while let Some(id) = at {
                let node = &self.nodes[id.0 as usize];
                if node.slot.inst().def(db) == trait_ {
                    return true;
                }
                at = node.parent;
            }
            false
        }

        /// The frame entry a use of `slot` reads: the innermost entry for exactly
        /// that slot, as written. A slot that would equal an opened one only for
        /// some generic arguments is a different slot.
        pub(super) fn lookup(
            &self,
            frame: Option<FrameId>,
            slot: SlotKey<'db>,
        ) -> Option<Choice<'db>> {
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
}
pub(super) use frames::ChoiceFrames;

/// Why an expression does not denote a choice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SourceError {
    /// Not a compile-time value.
    Runtime,
    /// A compile-time form trait choices do not support.
    Unsupported(&'static str),
    /// Already refused where the source was written; nothing more to report.
    Reported,
}

/// Whether `expr`'s callee body receives the call's decisions, by the
/// callee's type: a function item, however the callee is written (a
/// path, a binding, a block, a field), or a value whose type implements a
/// callable shape (`f(x)` means `f.call(x)`). A closure, or a function item
/// behind a capability, runs a body made elsewhere, which receives none.
/// `is_binding` says whether an expression names a local or a parameter,
/// `callee_ty` gives its type. Any other expression than a call counts as
/// direct (a method call, an operator). The one classification of call
/// edges.
pub(super) fn calls_directly<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    expr: ExprId,
    is_binding: impl Fn(ExprId) -> bool,
    callee_ty: impl Fn(ExprId) -> Option<TyId<'db>>,
) -> bool {
    match expr.data(db, body) {
        Partial::Present(Expr::Call(callee, _)) => {
            matches!(callee.data(db, body), Partial::Present(Expr::Path(..)))
                && !is_binding(*callee)
                || callee_ty(*callee).is_some_and(|ty| ty.is_func(db) || !is_function_value(db, ty))
        }
        _ => true,
    }
}

/// The goal `prove<G>` reads where it is written, whether it is called there
/// or taken as a value: its generic argument `G`, read through `resolve`.
/// The one statement of what `prove` reads, for its call (`decide_prove`)
/// and for its value (`call_uses`).
fn prove_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    callable: &Callable<'db>,
    mut resolve: impl FnMut(TyId<'db>) -> TyId<'db>,
) -> Option<TraitInstId<'db>> {
    let CallableDef::Func(func) = callable.callable_def() else {
        return None;
    };
    if !lib_func_matches(db, func, "core::obligations::prove") {
        return None;
    }
    match resolve(*callable.generic_args().first()?).data(db) {
        TyData::ConstraintTerm(goal) => Some(*goal),
        _ => None,
    }
}

/// How a recorded call is lowered (`TyChecker::record_call`).
pub(super) enum RecordedCall {
    /// A call of its callable.
    Call,
    /// A code-region intrinsic, with its region argument.
    CodeRegion(ExprId, super::CodeRegionIntrinsicKind),
    /// A compile-time intrinsic: `size_of`, `storage_slots`, `prove`.
    Const(super::ConstIntrinsicKind),
}

/// The operands of the call `expr`, in its callee's parameter order: a call's
/// arguments, a method call's receiver and then its arguments, an operator's
/// operands (an index's base and index, a compound assignment's target and
/// value). The one statement of which expression fills which parameter, read
/// where evidence is delivered (`record_call`) and where its absence is owed
/// (`owed_checks`).
pub(super) fn call_operands<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    expr: ExprId,
) -> Vec<ExprId> {
    match expr.data(db, body) {
        Partial::Present(Expr::Call(_, args)) => args.iter().map(|arg| arg.expr).collect(),
        Partial::Present(Expr::MethodCall(receiver, _, _, args)) => std::iter::once(*receiver)
            .chain(args.iter().map(|arg| arg.expr))
            .collect(),
        Partial::Present(Expr::Bin(lhs, rhs, _) | Expr::AugAssign(lhs, rhs, _)) => {
            vec![*lhs, *rhs]
        }
        Partial::Present(Expr::Un(operand, _)) => vec![*operand],
        _ => Vec::new(),
    }
}

/// A value whose call runs a function value's body: a closure or a function
/// item, behind any capability.
pub(super) fn is_function_value<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
    ty.is_func(db) || ty.as_closure(db).is_some()
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
    for (goal, choice, _) in body_trait_uses(db, typed) {
        if !uses.contains(&(goal, choice)) {
            uses.push((goal, choice));
        }
    }
    uses
}

/// One trait use a checked body records: the goal it reads, the decision
/// recorded for it, and whether it is a use's own slot (a trait constant's,
/// a trait call's) rather than a goal its callee or constant body declares.
type TraitUse<'db> = (TraitInstId<'db>, Option<Choice<'db>>, bool);

/// The trait uses behind `body_slot_uses`, each marked as a use's own slot or
/// a declared goal: each trait constant, each call (expression calls and a
/// `for` loop's protocol calls).
fn body_trait_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &super::TypedBody<'db>,
) -> Vec<TraitUse<'db>> {
    let mut uses = Vec::new();
    for cref in typed.const_refs() {
        let own = match cref {
            ConstRef::TraitConst(assoc) => Some(assoc.inst()),
            _ => None,
        };
        for (index, (goal, choice)) in const_use_reads(db, cref).into_iter().enumerate() {
            uses.push((slot_goal(db, goal), choice, own.is_some() && index == 0));
        }
    }
    let Some(body) = typed.body() else {
        return uses;
    };
    for (_, callable, _) in typed.recorded_calls(db) {
        if let Some(inst) = callable.trait_inst {
            uses.push((slot_goal(db, inst), callable.choice(), true));
        }
        let CallableDef::Func(func) = callable.callable_def() else {
            continue;
        };
        // The bounds of the body that runs, when it is known here, else the
        // callee's declared ones, each with what the call delivers to it.
        let (body_func, body_args) = callable
            .called_body(db, body.scope(), typed.assumptions())
            .unwrap_or_else(|| (func, callable.generic_args().to_vec()));
        let slots = body_deliveries(
            db,
            callable.choice().and_then(|c| c.known()),
            callable.delivered(),
            func,
            body_func,
        );
        for (bound, goal) in declared_goals(db, body_func, &body_args) {
            let delivered = slots
                .iter()
                .find_map(|(slot, choice)| (*slot == SlotRef::Bound(bound)).then_some(*choice));
            uses.push((slot_goal(db, goal), delivered, false));
        }
    }
    uses
}

/// The bound slots of `func` through which a goal its body leaves to
/// ordinary selection is proved: an implementation condition, at any depth,
/// on one of them (the uses' own slots are exempt, as in `call_decisions`).
/// A caller that delivers an opened or evidence choice into one is refused
/// (`OwedCheck::OpenedCondition`), as the call written under the `with`
/// is (`reads_opened_condition`).
pub(super) fn owed_opened_conditions<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    typed: &super::TypedBody<'db>,
) -> Vec<TraitInstId<'db>> {
    let own = own_slots(db, func);
    if own.is_empty() {
        return Vec::new();
    }
    let cx = TraitSolveCx::new(db, func.scope()).with_assumptions(typed.assumptions());
    let mut owed = Vec::new();
    let mut seen = Vec::new();
    for (goal, choice, own_slot) in body_trait_uses(db, typed) {
        if own_slot
            || choice.is_some_and(|c| !c.is_ordinary())
            || own.contains(&goal)
            || seen.contains(&goal)
        {
            continue;
        }
        seen.push(goal);
        let mut reads = Vec::new();
        let proof = ordinary_proof_reads(db, cx, goal, |condition| {
            let condition = slot_goal(db, condition);
            if own.contains(&condition) && !reads.contains(&condition) {
                reads.push(condition);
            }
            false
        });
        // A proof that cannot be followed is not known to avoid any slot.
        let reads = if proof == ProofReads::Nothing {
            reads
        } else {
            own.clone()
        };
        for slot in reads {
            if !owed.contains(&slot) {
                owed.push(slot);
            }
        }
    }
    owed
}

/// What an ordinary proof reads (`ordinary_proof_reads`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ProofReads {
    Nothing,
    /// An implementation condition the caller's `reads` accepts.
    Condition,
    /// A step that cannot be followed, which is never "reads nothing".
    Unfollowed,
}

/// Whether the ordinary proof of `goal` uses, at any depth, an implementation
/// condition `reads` accepts, or cannot be followed: a selection that is not
/// unique, or a walk past the limits (unknown is never "reads nothing").
/// Its step is a condition's depth below `goal`, as the owed walk's is a
/// body's depth below its call, so a proof with many conditions side by side
/// is not cut short. The one walk of ordinary proofs through conditions.
fn ordinary_proof_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
    mut reads: impl FnMut(TraitInstId<'db>) -> bool,
) -> ProofReads {
    let mut seen = rustc_hash::FxHashSet::default();
    let mut pending = vec![(goal, 0)];
    while let Some((goal, depth)) = pending.pop() {
        // Selection reads the slot; an associated-type equality left
        // unresolved by inference is not part of it.
        let goal = slot_goal(db, goal);
        if !seen.insert(goal) {
            continue;
        }
        // A callable shape of a function item or closure is built in: it has
        // no conditions to read.
        if goal
            .args(db)
            .first()
            .is_some_and(|ty| is_function_value(db, *ty))
        {
            continue;
        }
        // A goal still holding an inference variable cannot be selected
        // outside its body's table: it is not known to avoid the frame.
        if goal.args(db).iter().any(|ty| ty.has_var(db))
            || !choice_walk_step_within_limits(db, WalkStep::Proof, depth)
        {
            return ProofReads::Unfollowed;
        }
        let resolved = match resolve_trait_impl_instance(
            db,
            cx,
            goal,
            crate::analysis::ty::trait_choice::OrdinarySelection::decides_nothing(),
        ) {
            Selection::Unique(resolved) => resolved,
            _ => return ProofReads::Unfollowed,
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
            if reads(*condition) {
                return ProofReads::Condition;
            }
            pending.push((*condition, depth + 1));
        }
    }
    ProofReads::Nothing
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
            // The body that runs, or when it is chosen later the trait's
            // default body: the declaration every implementation's constant
            // is reached through, whose `Self` and supertrait slots are read
            // whichever body runs (`Self::P` in `const C = Self::P + 1`).
            let Some((body, _, subst)) = assoc
                .body_that_runs(db)
                .or_else(|| assoc.trait_default_body(db))
            else {
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

/// A type a callable's declaration states, with how a call instantiates it:
/// argument and return types through the callee's generic parameters, the
/// key of an effect it requires through its effect schema.
#[derive(Clone, Copy)]
enum DeclaredTy<'db> {
    Signature(TyId<'db>),
    EffectKey(TyId<'db>),
    EffectTrait(TraitInstId<'db>),
}

/// Every type a callable's declaration states, in its own generic
/// parameters: its argument and return types and the key of each effect it
/// requires (a type, a trait, or the trait of a row it uses). The call
/// refusal and type reach read a signature only through this list, so
/// no part of a signature is read by one and missed by the other.
fn declared_tys<'db>(db: &'db dyn HirAnalysisDb, def: CallableDef<'db>) -> Vec<DeclaredTy<'db>> {
    use crate::core::semantic::EffectRequirementKey;
    let mut tys = def
        .arg_tys(db)
        .iter()
        .map(|ty| DeclaredTy::Signature(*ty.skip_binder()))
        .collect::<Vec<_>>();
    tys.push(DeclaredTy::Signature(*def.ret_ty(db).skip_binder()));
    match def {
        CallableDef::Func(func) => {
            for requirement in func.effective_effect_requirements(db) {
                tys.push(match requirement.key {
                    EffectRequirementKey::Type(carrier) => DeclaredTy::EffectKey(carrier),
                    EffectRequirementKey::Trait(inst) => DeclaredTy::EffectTrait(inst),
                    EffectRequirementKey::Row(row) => DeclaredTy::EffectTrait(row.inst),
                    EffectRequirementKey::Other => continue,
                });
            }
        }
        CallableDef::VariantCtor(_) => {}
    }
    tys
}

/// The slots a trait instance's arguments and associated-type equalities
/// read.
fn inst_slot_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
) -> Vec<TraitInstId<'db>> {
    inst.args(db)
        .iter()
        .chain(inst.assoc_type_bindings(db).values())
        .flat_map(|ty| type_slot_reads(db, *ty))
        .collect()
}

/// The slots a callable's declared types read, in its own parameters: what
/// a caller that fills those parameters' slots reaches (type reach).
fn declared_slot_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    def: CallableDef<'db>,
) -> Vec<TraitInstId<'db>> {
    declared_tys(db, def)
        .into_iter()
        .flat_map(|declared| match declared {
            DeclaredTy::Signature(ty) | DeclaredTy::EffectKey(ty) => type_slot_reads(db, ty),
            DeclaredTy::EffectTrait(inst) => inst_slot_reads(db, inst),
        })
        .collect()
}

/// The slots a call's declared types read, instantiated for the call, and
/// the slots its generic arguments read.
fn call_slot_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    callable: &Callable<'db>,
) -> Vec<TraitInstId<'db>> {
    use crate::analysis::ty::effects::{instantiate_trait_effect_key, instantiate_type_effect_key};
    let mut reads = callable
        .generic_args()
        .iter()
        .flat_map(|ty| type_slot_reads(db, *ty))
        .collect::<Vec<_>>();
    let def = callable.callable_def();
    let has_effects = matches!(def, CallableDef::Func(func) if func.has_effects(db));
    let mut arg = 0;
    for declared in declared_tys(db, def) {
        match declared {
            DeclaredTy::Signature(_) => {
                let ty = if arg < def.arg_tys(db).len() {
                    callable.arg_ty(db, arg)
                } else {
                    Some(callable.ret_ty(db))
                };
                arg += 1;
                reads.extend(ty.into_iter().flat_map(|ty| type_slot_reads(db, ty)));
            }
            DeclaredTy::EffectKey(ty) if has_effects => {
                reads.extend(type_slot_reads(
                    db,
                    instantiate_type_effect_key(db, ty, callable),
                ));
            }
            DeclaredTy::EffectTrait(inst) if has_effects => {
                reads.extend(inst_slot_reads(
                    db,
                    instantiate_trait_effect_key(db, inst, callable),
                ));
            }
            DeclaredTy::EffectKey(_) | DeclaredTy::EffectTrait(_) => {}
        }
    }
    reads
}

/// Whether `ty` holds a constant refused where it is read
/// (`written_frames::refused_at_read`).
fn holds_refused_read<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    struct Holds<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Holds<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            if super::written_frames::refused_at_read(self.db, *const_ty) {
                self.found = true;
            }
            walk_const_ty(self, const_ty)
        }
    }
    let mut holds = Holds { db, found: false };
    ty.visit_with(&mut holds);
    holds.found
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
            // A constant refused where it is read is already a refusal; its
            // read is not counted again where the type is used.
            if super::written_frames::refused_at_read(db, *const_ty) {
                return;
            }
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
        // A type constructor not applied to all its arguments (`Cell` as the
        // `Self` of a trait over `* -> *`) has no field values to read.
        let args = ty.generic_args(db);
        if args.len() != adt.params(db).len() {
            continue;
        }
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
    /// opened: no stage could give it one meaning. `ty`
    /// is the type as inferred here, `written` the part written here (the
    /// same type, or a literal's path before its expected type fills it).
    pub(super) fn refuse_choice_dependent_ty(
        &mut self,
        written: TyId<'db>,
        ty: TyId<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        if !has_frames(frame) {
            return false;
        }
        // A type written here that holds a constant refused where it is read
        // is this refusal; one read elsewhere, such as an annotation's that a
        // literal's type takes, is not counted again (`type_slot_reads`).
        let dependent = holds_refused_read(self.db, written)
            || type_slot_reads(self.db, ty)
                .into_iter()
                .any(|slot| self.env.frame_choice(frame, slot).is_some());
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
        if !has_frames(frame) {
            return false;
        }
        let dependent = crate::analysis::ty::ty_lower::hir_ty_const_slot_reads(
            self.db,
            hir_ty,
            self.env.scope(),
            self.env.assumptions(),
        )
        .into_iter()
        .any(|slot| self.env.frame_choice(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
    }

    /// The written-argument refusal for a `with` header's key, read where
    /// the key is lowered: under the openings of the header's earlier
    /// bindings, as the binding's value is.
    pub(super) fn refuse_written_with_key(
        &mut self,
        key_path: crate::hir_def::PathId<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        if !has_frames(frame) {
            return false;
        }
        let dependent = crate::analysis::ty::ty_lower::hir_path_const_slot_reads(
            self.db,
            key_path,
            self.env.scope(),
            self.env.assumptions(),
        )
        .into_iter()
        .any(|slot| self.env.frame_choice(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
    }

    /// The same refusal for the constant arguments written in one
    /// expression's or pattern's own path or generic argument list, such as
    /// `Arr<{ Item::P }> {}`, `E<{ Item::P }>::A` or `g<{ Item::P }>`.
    /// Lowering evaluates them with the ordinary implementation before any
    /// later check sees the node, so the written syntax is the only place
    /// their reads show. This is the one scan of written arguments in a body:
    /// every expression and pattern passes through it once
    /// (`check_expr_with_result_context`, `check_pat`), and written types go
    /// through `lower_ty`. A refused expression is remembered so the check of
    /// its call or literal adds no second diagnostic.
    pub(super) fn refuse_written_const_args(&mut self, node: WrittenNode) -> bool {
        use crate::analysis::ty::ty_lower::{
            hir_generic_args_const_slot_reads, hir_path_const_slot_reads,
        };
        use crate::hir_def::Pat;
        if !has_frames(self.env.choice_frames.current()) {
            return false;
        }
        let (db, scope, assumptions) = (self.db, self.env.scope(), self.env.assumptions());
        let path_reads = |path: &Partial<crate::hir_def::PathId<'db>>| match path {
            Partial::Present(path) => hir_path_const_slot_reads(db, *path, scope, assumptions),
            Partial::Absent => Vec::new(),
        };
        let (reads, primary) = match node {
            WrittenNode::Expr(expr) => {
                let Partial::Present(data) = expr.data(db, self.body()) else {
                    return false;
                };
                let reads = match data {
                    Expr::Path(path) | Expr::RecordInit(path, _) => path_reads(path),
                    Expr::MethodCall(_, _, args, _) => {
                        hir_generic_args_const_slot_reads(db, *args, scope, assumptions)
                    }
                    // A header's keys are read where `check_with` lowers
                    // each, under the header's earlier openings like its
                    // values (`refuse_written_with_key`).
                    Expr::With(..) => Vec::new(),
                    // Types written in these go through `lower_ty`; an array
                    // repeat's length is checked unevaluated in
                    // `check_array_rep`.
                    Expr::Lit(_)
                    | Expr::Block(..)
                    | Expr::Closure { .. }
                    | Expr::Bin(..)
                    | Expr::Un(..)
                    | Expr::Cast(..)
                    | Expr::Try(_)
                    | Expr::Call(..)
                    | Expr::Assert(_)
                    | Expr::UnsupportedMacroCall
                    | Expr::Field(..)
                    | Expr::Tuple(_)
                    | Expr::Array(_)
                    | Expr::ArrayRep(..)
                    | Expr::If(..)
                    | Expr::Match(..)
                    | Expr::Assign(..)
                    | Expr::AugAssign(..) => Vec::new(),
                };
                (reads, expr.span(self.body()).into())
            }
            WrittenNode::Pat(pat) => {
                let Partial::Present(data) = pat.data(db, self.body()) else {
                    return false;
                };
                let reads = match data {
                    Pat::Path(path, _) | Pat::PathTuple(path, _) | Pat::Record(path, _) => {
                        path_reads(path)
                    }
                    Pat::WildCard | Pat::Rest | Pat::Lit(_) | Pat::Tuple(_) | Pat::Or(..) => {
                        Vec::new()
                    }
                };
                (reads, pat.span(self.body()).into())
            }
        };
        let refused = self.refuse_choice_dependent_written_args(reads, primary);
        if refused && let WrittenNode::Expr(expr) = node {
            self.env.written_refusals[expr] = true;
        }
        refused
    }

    fn refuse_choice_dependent_written_args(
        &mut self,
        reads: Vec<TraitInstId<'db>>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        let dependent = reads
            .into_iter()
            .any(|slot| self.env.frame_choice(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
    }

    /// A call whose declared types, instantiated here, read an opened slot
    /// (`call_slot_reads`). Applied early to path and method calls so the
    /// refusal is their only diagnostic; every registered call is read again
    /// when the body is finished (`record_choice_decisions`).
    pub(super) fn refuse_choice_dependent_signature(
        &mut self,
        callable: &Callable<'db>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let frame = self.env.choice_frames.current();
        if !has_frames(frame) {
            return false;
        }
        let dependent = call_slot_reads(self.db, callable)
            .into_iter()
            .any(|slot| self.env.frame_choice(frame, slot).is_some());
        if dependent {
            self.push_diag(BodyDiag::ChoiceDependentType { primary });
        }
        dependent
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
        use crate::analysis::name_resolution::path_resolver::{ImplNameRes, resolve_impl_name};
        // The name is found as any item's is: in scope, imported, or by a
        // module path, and visible here.
        let implementation = match resolve_impl_name(self.db, *path, self.env.scope()) {
            ImplNameRes::Found(implementation) => implementation,
            ImplNameRes::NotVisible => {
                return Some(Err(
                    "this implementation name is not visible here; an implementation name is private to its module unless declared `pub impl`",
                ));
            }
            ImplNameRes::Ambiguous => return Some(Err("the implementation name is ambiguous")),
            ImplNameRes::NotFound => return None,
        };
        // A bare name that is both a value and a named implementation is an
        // error that asks which one is meant; a value alone is a value.
        if path.as_ident(self.db).is_some() {
            let minter = LoweringContext::new();
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
                return Some(Err(
                    "this name is both a value and a named implementation; write `{ name }` for the value, or rename one of them",
                ));
            }
        }
        let implementation = &implementation;
        let Some(candidate) = lower_impl_trait(self.db, *implementation) else {
            return Some(Err("the named implementation declaration is invalid"));
        };
        if !candidate.params(self.db).is_empty() {
            return Some(Err(
                "a generic named implementation cannot be opened by name",
            ));
        }
        // Its conditions are proved where it is declared, not under the
        // frames of the opening: a condition that only
        // a named implementation answers is refused until sub-choices carry
        // the opening's answer into the implementation.
        Some(
            KnownChoice::prove(
                self.db,
                TraitSolveCx::new(self.db, implementation.scope()),
                *implementation,
                // The slot it answers; its associated types are its own
                // (compared with the ordinary ones where it is declared).
                slot_goal(self.db, candidate.trait_inst(self.db)),
            )
            .map_err(|_| {
                "the named implementation's conditions do not hold where it is declared; a condition only a named implementation answers is not supported yet"
            }),
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
            // effect value; it is refused, not ignored.
            if !self.holds_evidence(ty) {
                // An effect value; its type is read again once inference is
                // done, and a type parameter's is owed to callers.
                self.env.effect_with_operands.push(value);
                return None;
            }
            // A mutable place holding evidence was refused where it was bound.
            let refused_place = prop
                .binding
                .is_some_and(|binding| self.is_mutable_evidence_place(&binding, ty));
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
                resolve_trait_impl_instance(
                    self.db,
                    self.env.proof_context(),
                    goal,
                    crate::analysis::ty::trait_choice::OrdinarySelection::decides_nothing(),
                ),
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
                    mode: crate::hir_def::params::FuncParamMode::Mut,
                    ..
                }
                | LocalBinding::Param {
                    site: ParamSite::ContractInit(_),
                    ..
                } => Err(SourceError::Reported),
                LocalBinding::Param { idx, .. } => {
                    Ok(Choice::Slot(SlotRef::EvidenceParam(idx as u16)))
                }
                LocalBinding::Local { pat, .. } => match self.env.binding_choice(pat) {
                    Some(choice) => choice,
                    // Bound inside a destructuring pattern: taken out of an
                    // aggregate, as a field access is.
                    None if self.destructured(pat) => Err(SourceError::Unsupported(AGGREGATE)),
                    None => Err(SourceError::Runtime),
                },
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
                self.factory_choice(expr, &callable, args)
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
            // A read access of an evidence place means what the place means.
            Partial::Present(Expr::Un(inner, crate::hir_def::UnOp::Ref)) => {
                self.denote_tail(*inner)
            }
            // A branch chooses evidence by a value.
            Partial::Present(Expr::If(..) | Expr::Match(..)) => Err(SourceError::Runtime),
            Partial::Present(Expr::Field(base, _) | Expr::Bin(base, _, BinOp::Index)) => {
                // A field of a refused mutable holder was reported there.
                if let Some(LocalBinding::Local { pat, .. }) =
                    self.env.typed_expr(*base).and_then(|prop| prop.binding)
                    && self.env.binding_choice(pat) == Some(Err(SourceError::Reported))
                {
                    return Err(SourceError::Reported);
                }
                Err(SourceError::Unsupported(AGGREGATE))
            }
            _ => Err(SourceError::Runtime),
        }
    }

    /// Whether the binding `pat` sits inside a tuple, record or variant
    /// pattern (through `|` alternatives), so its value is taken out of an
    /// aggregate.
    fn destructured(&self, pat: PatId) -> bool {
        use crate::hir_def::Pat;
        let parent = self
            .body()
            .pats(self.db)
            .iter()
            .find_map(|(parent, data)| match data {
                Partial::Present(Pat::Tuple(parts) | Pat::PathTuple(_, parts))
                    if parts.contains(&pat) =>
                {
                    Some((parent, true))
                }
                Partial::Present(Pat::Record(_, fields))
                    if fields.iter().any(|field| field.pat == pat) =>
                {
                    Some((parent, true))
                }
                Partial::Present(Pat::Or(lhs, rhs)) if *lhs == pat || *rhs == pat => {
                    Some((parent, false))
                }
                _ => None,
            });
        match parent {
            Some((_, true)) => true,
            Some((alternative, false)) => self.destructured(alternative),
            None => false,
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
        call: ExprId,
        callable: &Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
    ) -> Result<Choice<'db>, SourceError> {
        let (key, values) = self.closed_call(call, callable, args)?;
        evidence_choice(self.db, Some(eval_instance_with_args(self.db, key, values)))
    }

    /// A closed const call evaluated while checking: the instance the same
    /// call gets anywhere (`checked_call_key`), with the decisions recorded
    /// where it is written (`decided_call`, wherever its value is used), and
    /// its closed argument values. One meaning for one call.
    fn closed_call(
        &mut self,
        call: ExprId,
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
        // Delivered here: the closed evidence arguments' values, then the
        // recorder's decisions.
        callable.set_delivered(Vec::new());
        let (decided, refusals) = self
            .env
            .decided_call(self.env.use_frames[call], callable, true);
        if !refusals.is_empty() || matches!(decided.choice(), Some(Choice::Slot(_))) {
            return Err(unknown);
        }
        callable = decided;
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
        delivered.extend(callable.delivered().iter().copied());
        callable.set_delivered(delivered);
        let (scope, assumptions) = (self.env.scope(), self.env.assumptions());
        let Some((body, _)) = callable.called_body(self.db, scope, assumptions) else {
            return Err(unknown);
        };
        if body_deliveries(
            self.db,
            callable.choice().and_then(|c| c.known()),
            callable.delivered(),
            func,
            body,
        )
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
    /// argument is not closed: a runtime input, or a compile-time form trait
    /// choices do not support.
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
                        let frame = self.env.use_frames[expr];
                        let Ok(decided) =
                            self.env
                                .trait_const_decisions(frame, assoc, &mut Vec::new())
                        else {
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
                    let (key, values) = self.closed_call(expr, &callable, args)?;
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
    /// implementation.
    pub(super) fn pattern_constant(&self, cref: ConstRef<'db>) -> Option<ConstRef<'db>> {
        let frame = self.env.choice_frames.current();
        let (decided, own, delivered) = match cref {
            ConstRef::TraitConst(use_) => {
                let decided = self
                    .env
                    .trait_const_decisions(frame, use_, &mut Vec::new())
                    .ok()?;
                (
                    ConstRef::TraitConst(decided),
                    decided.choice(),
                    decided.delivered(),
                )
            }
            ConstRef::InherentConst(use_) => {
                let decided = self
                    .env
                    .inherent_const_decisions(frame, use_, &mut Vec::new())
                    .ok()?;
                (ConstRef::InherentConst(decided), None, decided.delivered())
            }
            ConstRef::Const(_) => return Some(cref),
        };
        // Pending evidence, or a slot a written constant reads unsupplied
        // under the frames around it (`seed_written_frames`): not known here.
        let unknown = |choice: Choice<'db>| {
            choice.evidence_param().is_some() || choice == Choice::Slot(SlotRef::Unsupplied)
        };
        let pending = own.is_some_and(unknown)
            || delivered.is_some_and(|list| list.items(self.db).iter().any(|(_, c)| unknown(*c)));
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
        let Some(binding @ LocalBinding::Local { pat, .. }) = self.env.pat_binding(pat) else {
            return;
        };
        let ty = self.resolved_ty(prop.ty);
        if self.is_mutable_evidence_place(&binding, ty) {
            // No mutable place holds evidence, in a field or not.
            self.push_diag(BodyDiag::EvidenceMutableStorage {
                primary: pat.span(self.body()).into(),
            });
            self.env.set_binding_choice(pat, Err(SourceError::Reported));
            return;
        }
        if evidence_goal(self.db, self.env.scope(), ty).is_none() {
            return;
        }
        // A mutable binding of evidence was refused above.
        let choice = self.denote_evidence(init, prop);
        self.env.set_binding_choice(pat, choice);
    }

    /// Parameters that cannot hold evidence: a mutable one, since no mutable
    /// place holds evidence, and one of a contract's `init`, which has no
    /// compile-time caller to choose it.
    pub(super) fn refuse_evidence_params(&mut self) {
        let params = self.env.params().to_vec();
        for binding in params {
            let LocalBinding::Param { ty, site, .. } = binding else {
                continue;
            };
            if !self.holds_evidence(ty) {
                continue;
            }
            let primary = binding.def_span(&self.env);
            if matches!(site, ParamSite::ContractInit(_)) {
                self.push_diag(BodyDiag::EvidenceChoiceNotCompileTime { primary });
            } else if self.is_mutable_evidence_place(&binding, ty) {
                self.push_diag(BodyDiag::EvidenceMutableStorage { primary });
            }
        }
    }

    /// Whether a value of type `ty` holds evidence, itself or in a field,
    /// element or borrow, at any depth. A record's fields are read as
    /// declared, with its arguments read too, so a record that contains a
    /// growing instance of itself is read in finitely many steps.
    fn holds_evidence(&self, ty: TyId<'db>) -> bool {
        holds_evidence(self.db, self.env.scope(), ty)
    }

    /// The one rule for mutable evidence places: a mutable binding
    /// or a mutable access (a `mut` parameter, `let x = mut p`) of a value
    /// that holds evidence, or a mutable borrow of one.
    fn is_mutable_evidence_place(&self, binding: &LocalBinding<'db>, ty: TyId<'db>) -> bool {
        let mutable = binding.is_mut()
            || self
                .env
                .binding_access(binding)
                .is_some_and(|access| access.is_mut());
        (mutable && self.holds_evidence(ty))
            || matches!(
                ty.as_capability(self.db),
                Some((crate::analysis::ty::ty_def::CapabilityKind::Mut, inner))
                    if self.holds_evidence(inner)
            )
    }

    /// Records the call `expr`. Every call form is recorded here and nowhere
    /// else: a path, method or value call, a method chosen after the body is
    /// checked, an operator, an index, a compound assignment and an
    /// intrinsic. Its evidence operands (`call_operands`) are delivered in
    /// parameter order, so no form can skip delivery; a call of a callable
    /// value refuses them instead.
    pub(super) fn record_call(
        &mut self,
        expr: ExprId,
        mut callable: Callable<'db>,
        form: RecordedCall,
    ) {
        let operands = call_operands(self.db, self.body(), expr);
        if self.env.is_value_call(expr) {
            self.refuse_evidence_value_call_args(&operands);
        } else {
            self.deliver_evidence_args(&mut callable, &operands);
        }
        match form {
            RecordedCall::Call => self.env.register_semantic_call(expr, callable),
            RecordedCall::CodeRegion(region_arg, kind) => self
                .env
                .register_code_region_intrinsic(expr, callable, region_arg, kind),
            RecordedCall::Const(kind) => self.env.register_const_intrinsic(expr, callable, kind),
        }
    }

    /// Records the `for` loop `stmt`'s plan, whose protocol calls are calls
    /// written at the loop. They take the driver, the base and the loop's
    /// state, which is a value the loop makes, not one written here: a state
    /// that holds evidence would reach its calls with no choice, so it is
    /// refused.
    pub(super) fn record_loop_plan(&mut self, stmt: StmtId, plan: ForLoopPlan<'db>) {
        if self.holds_evidence(plan.state_ty) {
            self.push_diag(BodyDiag::ObligationOpening {
                primary: plan.base.span(self.body()).into(),
                reason: "a `for` loop whose state holds evidence is not supported yet; its protocol calls would receive it with no choice",
            });
        }
        self.env.register_for_loop_plan(stmt, plan);
    }

    /// A call of a callable value (a closure, or a function reached through
    /// a block) runs a body that receives no decisions from the call: an
    /// argument that holds evidence is refused, not delivered.
    fn refuse_evidence_value_call_args(&mut self, args: &[ExprId]) {
        for &arg in args {
            let Some(prop) = self.env.typed_expr(arg) else {
                continue;
            };
            let ty = self.resolved_ty(prop.ty);
            if self.holds_evidence(ty) {
                self.push_diag(BodyDiag::ObligationOpening {
                    primary: arg.span(self.body()).into(),
                    reason: "evidence cannot be passed to a callable value yet; call the function by name",
                });
            }
        }
    }

    /// Denote the evidence operands of a call, in parameter order. A method
    /// call's receiver is its parameter 0, whichever way the method is
    /// resolved: a receiver of evidence type is an evidence parameter of the
    /// method, as it is when the method is called by its path.
    fn deliver_evidence_args(&mut self, callable: &mut Callable<'db>, operands: &[ExprId]) {
        let mut delivered = Vec::new();
        for (param, &expr) in operands.iter().enumerate() {
            let Some(prop) = self.env.typed_expr(expr) else {
                continue;
            };
            let ty = self.resolved_ty(prop.ty);
            if evidence_goal(self.db, self.env.scope(), ty).is_none() {
                continue;
            }
            match self.denote_evidence(expr, &prop) {
                Ok(choice) => delivered.push((Delivery::Param(param as u16), choice)),
                Err(error) => self.report_source_error(expr.span(self.body()).into(), error),
            }
        }
        callable.set_delivered(delivered);
    }

    /// `prove<G>()` answers `G` where it is written: from the frame in force,
    /// otherwise with the ordinary implementation of a concrete `G`.
    pub(super) fn decide_prove(&mut self, expr: ExprId, callable: &mut Callable<'db>) -> bool {
        let goal = prove_goal(self.db, callable, |term| self.resolved_ty(term));
        let result = match goal {
            None => Err("expected one trait goal"),
            Some(goal) => {
                let slot = self.env.normalized_slot(goal).inst();
                match self
                    .env
                    .frame_choice(self.env.choice_frames.current(), goal)
                {
                    Some(choice) if self.choice_answers(choice, goal) => Ok(choice),
                    Some(_) => Err("the opened implementation does not prove the requested goal"),
                    None if slot.args(self.db).iter().any(|ty| ty.has_param(self.db)) => {
                        Err("a generic goal can be proved only under a `with` that opens it")
                    }
                    // Proved through a condition on an opened slot, or by a
                    // proof that cannot be followed: refused as the call form
                    // of the same goal is.
                    None => match self.env.reads_opened_condition(
                        self.env.choice_frames.current(),
                        [goal],
                        None,
                    ) {
                        Some(refusal) => {
                            self.push_diag(BodyDiag::ObligationOpening {
                                primary: expr.span(self.body()).into(),
                                reason: match refusal {
                                    ChoiceRefusal::OpenedCondition => {
                                        "the goal is proved through an implementation with a condition on a slot opened here; conditional implementations under `with` are not supported yet"
                                    }
                                    _ => {
                                        "the goal's proof cannot be followed to one implementation while a slot is opened here, so it is not known to avoid that slot; this is not supported yet"
                                    }
                                },
                            });
                            return false;
                        }
                        None => self.ordinary_choice(goal).map(Choice::Known),
                    },
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
    /// whose associated types every named one shares.
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
        let Selection::Unique(resolved) = resolve_trait_impl_instance(
            self.db,
            cx,
            goal,
            crate::analysis::ty::trait_choice::OrdinarySelection::no_recorded_choice(),
        ) else {
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
    /// The choice `frame` gives the slot of `inst`, its arguments normalized
    /// in this body, so that every spelling of one slot reads one entry. The
    /// one reader of choice frames.
    pub(super) fn frame_choice(
        &self,
        frame: Option<FrameId>,
        inst: TraitInstId<'db>,
    ) -> Option<Choice<'db>> {
        // Only a slot of a trait some frame opened can be found; any other is
        // answered without normalizing it.
        if !self
            .choice_frames
            .opens_trait(self.db(), frame, inst.def(self.db()))
        {
            return None;
        }
        self.choice_frames.lookup(frame, self.normalized_slot(inst))
    }

    /// Whether `frame` opens a slot of `trait_`.
    pub(super) fn frame_opens_trait(
        &self,
        frame: Option<FrameId>,
        trait_: crate::hir_def::Trait<'db>,
    ) -> bool {
        self.choice_frames.opens_trait(self.db(), frame, trait_)
    }

    /// Whether `frame` answers all of `goal`: inside the block that opened
    /// its slot, the goal holds even with no ordinary implementation, by a
    /// known choice (whose conditions were checked when it was opened, and
    /// only with the associated types it has) or by evidence.
    pub(super) fn frame_proves(&self, frame: Option<FrameId>, goal: TraitInstId<'db>) -> bool {
        let db = self.db();
        match self.frame_choice(frame, goal) {
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
        let slot = self.normalized_slot(inst).inst();
        if let Some(found) = self.frame_choice(frame, inst) {
            return Some(found);
        }
        // A constant body written under a `with (e)` whose evidence type is
        // not written down: any slot that can be opened is unsupplied.
        if self.written_unknown
            && super::written_frames::has_named_implementation(db, self.scope(), slot)
        {
            return Some(Choice::Slot(SlotRef::Unsupplied));
        }
        let written = slot_goal(db, inst);
        if let Some(own) = self
            .own_bounds()
            .into_iter()
            .find(|own| *own == written || *own == slot)
        {
            return Some(Choice::Slot(SlotRef::Bound(own)));
        }
        None
    }

    /// The slots this body's own bounds are, in slot form: a function's
    /// `own_slots` (its bounds and its trait providers' bounds), or the bounds
    /// a constant body's item declares.
    pub(super) fn own_bounds(&self) -> Vec<TraitInstId<'db>> {
        let db = self.db();
        if let super::BodyOwner::Func(func, _) = self.owner() {
            return own_slots(db, func);
        }
        let mut own = Vec::new();
        for bound in self.base_assumptions().list(db) {
            let bound = slot_goal(db, *bound);
            if bound.args(db).iter().any(|ty| ty.has_param(db)) && !own.contains(&bound) {
                own.push(bound);
            }
        }
        own
    }

    /// `inst`'s slot as frames key it in this body (`SlotKey`).
    fn normalized_slot(&self, inst: TraitInstId<'db>) -> SlotKey<'db> {
        slot_key(self.db(), inst, self.scope(), self.assumptions())
    }

    /// A constant body written inside a function's body (a constant generic
    /// argument, an array length) receives nothing for the slots the frames
    /// around it open: they are unsupplied there, never answered the ordinary
    /// way, so no evaluation of it reads the ordinary implementation where
    /// the code around it reads the choice.
    pub(super) fn seed_written_frames(&mut self, body: crate::hir_def::Body<'db>) {
        let frames = super::written_frames::written_frames(self.db(), body);
        for slot in frames.slots() {
            let key = self.normalized_slot(*slot);
            let unsupplied = Choice::Slot(SlotRef::Unsupplied);
            if !has_frames(self.choice_frames.current()) {
                self.choice_frames.seed(key, unsupplied);
            } else {
                let _ = self.choice_frames.push(None, key, unsupplied);
            }
        }
        self.written_unknown = frames.unknown();
    }

    /// A named implementation's own body sees itself for its own slot, for
    /// every use of that slot in the body, `Self` forms, receiver calls and
    /// concrete paths alike. A trait's default body run for it reads it
    /// through its `Self` forms.
    pub(super) fn seed_own_choice(&mut self, implementation: crate::hir_def::ImplTrait<'db>) {
        let db = self.db();
        let Some(implementor) = lower_impl_trait(db, implementation) else {
            return;
        };
        let goal = implementor.trait_inst(db);
        let slot = self.normalized_slot(goal);
        self.choice_frames.seed(
            slot,
            Choice::Known(KnownChoice::own(db, implementation, goal)),
        );
    }

    /// The ordinary implementation of `inst`, together with the choices for
    /// the bounds its `impl` declares, when `inst` has a unique authored
    /// ordinary implementation in this body's proof context. A bound of the
    /// `impl` that is one of this body's own bounds records `SlotRef::Bound`,
    /// and a cyclic goal on the current proof path is left to the body's
    /// slot resolution so coinductive proofs terminate.
    fn ordinary_choice(
        &self,
        inst: TraitInstId<'db>,
        depth: usize,
        active: &mut rustc_hash::FxHashSet<TraitInstId<'db>>,
    ) -> Option<Choice<'db>> {
        let db = self.db();
        let written = slot_goal(db, inst);
        let slot = self.normalized_slot(inst).inst();
        if let Some(own) = self
            .own_bounds()
            .into_iter()
            .find(|own| *own == written || *own == slot)
        {
            return Some(Choice::Slot(SlotRef::Bound(own)));
        }
        if slot.args(db).iter().any(|ty| ty.has_var(db))
            || !choice_walk_step_within_limits(db, WalkStep::Proof, depth)
            || !active.insert(slot)
        {
            return None;
        }
        let cx = self.proof_context();
        let resolved = match resolve_trait_impl_instance(
            db,
            cx,
            slot,
            crate::analysis::ty::trait_choice::OrdinarySelection::no_recorded_choice(),
        ) {
            Selection::Unique(resolved) => resolved,
            _ => {
                active.remove(&slot);
                return None;
            }
        };
        let ImplementorOrigin::Hir(implementation) = resolved.selected().origin(db) else {
            active.remove(&slot);
            return None;
        };
        let mut bounds = Vec::new();
        for (bound, at_use) in impl_declared_goals(db, implementation, resolved.impl_args(db)) {
            if let Some(choice) = self.ordinary_choice(at_use, depth + 1, active) {
                let item = (Delivery::Bound(bound), choice);
                if !bounds.contains(&item) {
                    bounds.push(item);
                }
            }
        }
        active.remove(&slot);
        let bounds = (!bounds.is_empty()).then(|| DeliveryList::new(db, bounds));
        Some(Choice::Known(KnownChoice::ordinary(
            db,
            implementation,
            slot,
            bounds,
        )))
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
            .and_then(|inst| self.decide(frame, inst))
            .or_else(|| {
                let inst = callable.trait_inst?;
                let mut active = rustc_hash::FxHashSet::default();
                self.ordinary_choice(inst, 0, &mut active)
            });
        let generic = !self.own_bounds().is_empty();
        let CallableDef::Func(func) = callable.callable_def() else {
            return Ok((own, Vec::new()));
        };
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
        // it would if it were known here. A candidate that applies only
        // for some generic arguments has no bound written here, so the frame
        // does not reach it (the written reading).
        let mut candidate_deliveries = Vec::new();
        let mut undecided_candidates = Vec::new();
        if known.is_none() && (has_frames(frame) || generic) {
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
        if let Some(refusal) = self.reads_opened_condition(frame, undecided, own_slot) {
            return Err(refusal);
        }
        let mut delivered: Vec<(Delivery<'db>, Choice<'db>)> = Vec::new();
        for (bound, at_call, choice) in decided {
            let choice = choice.or_else(|| {
                let mut active = rustc_hash::FxHashSet::default();
                self.ordinary_choice(at_call, 0, &mut active)
            });
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

    /// The slot `inst` as written, normalized here, whose implementations a
    /// use chosen later could run. None for a goal that still holds inference
    /// variables.
    fn candidate_slot(&self, inst: TraitInstId<'db>) -> Option<TraitInstId<'db>> {
        let inst = self.normalized_slot(inst).inst();
        (!inst.args(self.db()).iter().any(|ty| ty.has_var(self.db()))).then_some(inst)
    }

    /// The method bodies a trait call whose implementation is chosen later
    /// could run, with their arguments (`trait_def::candidate_methods`).
    fn candidate_bodies(
        &self,
        callable: &Callable<'db>,
        nominal: crate::hir_def::Func<'db>,
    ) -> Vec<(crate::hir_def::Func<'db>, Vec<TyId<'db>>)> {
        let db = self.db();
        let (Some(inst), Some(name)) = (callable.trait_inst, nominal.name(db).to_opt()) else {
            return Vec::new();
        };
        let Some(inst) = self.candidate_slot(inst) else {
            return Vec::new();
        };
        let ingot = self.scope().top_mod(db).ingot(db);
        let mut bodies = Vec::new();
        for method in crate::analysis::ty::trait_def::candidate_methods(
            db,
            ingot,
            inst,
            name,
            callable.mode(),
        ) {
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

    /// The refusal of a bound left to its callee's ordinary selection, other
    /// than `exempt` (a use's own slot), that is proved through a condition
    /// on a slot `frame` opened, or whose proof cannot be followed there.
    fn reads_opened_condition(
        &self,
        frame: Option<FrameId>,
        undecided: impl IntoIterator<Item = TraitInstId<'db>>,
        exempt: Option<TraitInstId<'db>>,
    ) -> Option<ChoiceRefusal> {
        if !has_frames(frame) {
            return None;
        }
        let mut unfollowed = false;
        for goal in undecided {
            if Some(goal) == exempt {
                continue;
            }
            match self.conditions_read_frame(frame, goal) {
                ProofReads::Condition => return Some(ChoiceRefusal::OpenedCondition),
                ProofReads::Unfollowed => unfollowed = true,
                ProofReads::Nothing => {}
            }
        }
        unfollowed.then_some(ChoiceRefusal::UnfollowedProof)
    }

    /// Whether the ordinary proof of `goal` uses, at any depth, an
    /// implementation condition whose slot `frame` opened. Computed once per
    /// frame and goal: a body's uses ask about the same proofs again.
    fn conditions_read_frame(&self, frame: Option<FrameId>, goal: TraitInstId<'db>) -> ProofReads {
        let Some(frame_id) = frame else {
            return ProofReads::Nothing;
        };
        if let Some(known) = self.conditions_read.borrow().get(&(frame_id, goal)) {
            return *known;
        }
        let reads = self.walk_conditions(frame_id, goal);
        self.conditions_read
            .borrow_mut()
            .insert((frame_id, goal), reads);
        reads
    }

    /// The walk behind `conditions_read_frame`: the ordinary proof as it
    /// runs in `frame`, whose opened goals are premises there as they were
    /// while the block was checked (`add_premise_if_needed`).
    fn walk_conditions(&self, frame: FrameId, goal: TraitInstId<'db>) -> ProofReads {
        let db = self.db();
        let mut premises = self.assumptions().list(db).to_vec();
        for opened in self.choice_frames.opened(Some(frame)) {
            if !premises.contains(&opened) {
                premises.push(opened);
            }
        }
        let cx = TraitSolveCx::new(db, self.scope())
            .with_assumptions(PredicateListId::new(db, premises).extend_all_bounds(db));
        ordinary_proof_reads(db, cx, goal, |condition| {
            self.frame_choice(Some(frame), condition).is_some()
        })
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
        uses.extend(prove_goal(db, callable, |term| term));
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

    /// One call's decisions, recorded where it is written (`frame`), whatever
    /// its form: an expression call, a value call, a `for` loop's protocol
    /// call, or a factory call denoted later (`closed_call`). A call whose
    /// callee body receives decisions (`calls_directly`) gets its own slot's
    /// and its body's bounds'; a call of a function value gets none. Also
    /// refused: a declared type that reads an opened slot.
    pub(super) fn decided_call(
        &self,
        frame: Option<FrameId>,
        mut callable: Callable<'db>,
        direct: bool,
    ) -> (Callable<'db>, Vec<ChoiceRefusal>) {
        let db = self.db();
        let mut refusals = Vec::new();
        if has_frames(frame)
            && call_slot_reads(db, &callable)
                .into_iter()
                .any(|slot| self.frame_choice(frame, slot).is_some())
        {
            refusals.push(ChoiceRefusal::DependentType);
        }
        if direct {
            let (own, decided) = match self.call_decisions(frame, &callable) {
                Ok(decisions) => decisions,
                Err(refusal) => {
                    refusals.push(refusal);
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
        // The body read here for a concrete goal is what the instance
        // must run (checked where the instance is built, `const_ref`).
        let concrete = callable.trait_inst().is_none_or(|inst| {
            !inst
                .args(db)
                .iter()
                .any(|ty| ty.has_param(db) || ty.has_var(db))
        });
        if concrete && !matches!(callable.choice(), Some(Choice::Slot(_))) {
            let body = callable
                .called_body(db, self.scope(), self.assumptions())
                .map(|(body, _)| body);
            callable.set_checked_body(body);
        }
        (callable, refusals)
    }

    /// Record each trait use's decision, each direct call's deliveries, and
    /// the choices each function value would carry. A use whose slot could
    /// coincide with an opened slot without being it is recorded as
    /// ambiguous and refused. Runs once inference has resolved every goal.
    pub(super) fn record_choice_decisions(&mut self) {
        let db = self.db();
        let callee_only = self.callee_only_values();
        let path_callees = callee_only
            .iter()
            .map(|(value, _)| *value)
            .collect::<Vec<_>>();
        // Where no frame is in force at the value or any of its calls, the
        // value's conditions are checked where it is written; otherwise its
        // calls check them, under their own frames.
        for (value, calls) in callee_only {
            if has_frames(self.use_frames[value])
                || calls.iter().any(|call| has_frames(self.use_frames[*call]))
            {
                self.callee_only_values.push(value);
                self.callee_only_calls.extend(calls);
            }
        }
        // A call through a function value made elsewhere receives nothing from
        // the frame where it is called; only a path naming the function does.
        let is_binding = |callee| {
            self.typed_expr(callee)
                .is_some_and(|prop| prop.binding.is_some())
        };
        let calls = self
            .semantic_expr_lowering
            .iter()
            .filter_map(|(expr, lowering)| match lowering {
                Some(SemanticExprLowering::Call { callable }) => Some((
                    expr,
                    callable.clone(),
                    calls_directly(db, self.body(), expr, is_binding, |callee| {
                        self.typed_expr(callee).map(|prop| prop.ty)
                    }),
                )),
                _ => None,
            })
            .collect::<Vec<_>>();
        for (expr, callable, direct) in calls {
            let (callable, refusals) = self.decided_call(self.use_frames[expr], callable, direct);
            self.choice_refusals
                .extend(refusals.into_iter().map(|refusal| (expr, refusal)));
            self.semantic_expr_lowering[expr] = Some(SemanticExprLowering::Call { callable });
        }
        // A `for` loop's protocol calls are calls written at the loop.
        let loops = self
            .for_loop_plans
            .iter()
            .filter_map(|(stmt, plan)| Some((stmt, plan.as_ref()?.base)))
            .collect::<Vec<_>>();
        for (stmt, written) in loops {
            let mut plan = self.for_loop_plans[stmt].clone().unwrap();
            for call in &mut plan.calls {
                let (callable, refusals) =
                    self.decided_call(self.use_frames[written], call.callable.clone(), true);
                for refusal in refusals {
                    if !self.choice_refusals.contains(&(written, refusal)) {
                        self.choice_refusals.push((written, refusal));
                    }
                }
                call.callable = callable;
            }
            self.for_loop_plans[stmt] = Some(plan);
        }
        // A function value is a path naming a function that reaches more than
        // the callees of calls. It would carry the decisions for the slots it
        // reads.
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
            let mut undecided = Vec::new();
            for goal in self.call_uses(&callable) {
                match self.decide(frame, goal) {
                    Some(choice) => {
                        if !captured.contains(&choice) {
                            captured.push(choice);
                        }
                    }
                    None => undecided.push(goal),
                }
            }
            // A function value's bounds are left to ordinary selection as a
            // call's are, and refused as a call's are when proved through a
            // condition on an opened slot.
            let own_slot = callable.trait_inst.map(|inst| slot_goal(db, inst));
            if let Some(refusal) = self.reads_opened_condition(frame, undecided, own_slot) {
                self.choice_refusals.push((expr, refusal));
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
            let mut reached = Vec::new();
            let decided = match self.const_refs[expr] {
                Some(ConstRef::TraitConst(use_)) => self
                    .trait_const_decisions(frame, use_, &mut reached)
                    .map(ConstRef::TraitConst),
                Some(ConstRef::InherentConst(use_)) => self
                    .inherent_const_decisions(frame, use_, &mut reached)
                    .map(ConstRef::InherentConst),
                _ => continue,
            };
            for goal in reached {
                if !self.const_reaches.contains(&goal) {
                    self.const_reaches.push(goal);
                }
            }
            match decided {
                Ok(decided) => self.const_refs[expr] = Some(decided),
                Err(refusal) => self.choice_refusals.push((expr, refusal)),
            }
        }
        // A `with` operand taken as an effect value because its type was not
        // known yet is refused if it is evidence.
        for expr in self.effect_with_operands.clone() {
            if self
                .typed_expr(expr)
                .is_some_and(|prop| holds_evidence(db, self.scope(), prop.ty))
            {
                self.choice_refusals
                    .push((expr, ChoiceRefusal::UninferredEvidence));
            }
        }
        // A closure's body runs as its own instance, which receives none of
        // its enclosing function's slots. The slot choices its uses read are
        // recorded: a bound it reads is treated as reached, so no caller may
        // deliver an opened choice into it, and an evidence choice it reads
        // is refused (`check_choice_uses`).
        let closure_uses = self
            .closure_exprs
            .iter()
            .filter_map(|(expr, inside)| inside.then_some(expr))
            .collect::<Vec<_>>();
        for expr in closure_uses {
            let mut slots = Vec::new();
            for choice in self.choices_read_at(expr) {
                if matches!(choice, Choice::Slot(_)) && !slots.contains(&choice) {
                    slots.push(choice);
                }
            }
            if !slots.is_empty() {
                self.closure_choices.push((expr, slots));
            }
        }
        // A closure that reads a choice a frame answers and needs an effect
        // provider is refused: the choice is read where the closure is written,
        // a provider where it is called, and the two could disagree.
        let mut mixed = Vec::new();
        for (expr, closure) in self.closure_of.iter() {
            let Some(closure) = *closure else {
                continue;
            };
            let frame = self.use_frames[expr];
            if !has_frames(frame)
                || mixed.contains(&closure)
                || !self.closure_needs_provider(closure)
            {
                continue;
            }
            if self.choices_read_at(expr).into_iter().any(|choice| {
                matches!(choice, Choice::Known(known)
                    if self.frame_choice(frame, known.goal()) == Some(choice))
            }) {
                mixed.push(closure);
            }
        }
        self.choice_refusals.extend(
            mixed
                .into_iter()
                .map(|closure| (closure, ChoiceRefusal::ClosureChoiceAndProvider)),
        );
    }

    /// The paths naming a function that reach only the callees of calls,
    /// which decide for it where they are written: a path callee, the
    /// value a block callee yields, and a `let` binding's initializer when
    /// every use of the binding is a callee. Such a path carries nothing as
    /// a value. Each comes with the calls it reaches.
    fn callee_only_values(&self) -> Vec<(ExprId, Vec<ExprId>)> {
        use crate::hir_def::Stmt;
        let db = self.db();
        let body = self.body();
        let is_binding = |expr| {
            self.typed_expr(expr)
                .is_some_and(|prop| prop.binding.is_some())
        };
        // The expression a block yields, through nested blocks.
        let yielded = |mut expr: ExprId| loop {
            let Partial::Present(Expr::Block(stmts, _)) = expr.data(db, body) else {
                return expr;
            };
            let Some(Partial::Present(Stmt::Expr(tail))) =
                stmts.last().map(|stmt| stmt.data(db, body))
            else {
                return expr;
            };
            expr = *tail;
        };
        let names_function = |expr: ExprId| {
            matches!(expr.data(db, body), Partial::Present(Expr::Path(..))) && !is_binding(expr)
        };
        let calls = body
            .exprs(db)
            .iter()
            .filter_map(|(call, data)| match data {
                Partial::Present(Expr::Call(callee, _)) => Some((*callee, call)),
                _ => None,
            })
            .collect::<Vec<_>>();
        let call_of = |callee: ExprId| {
            calls
                .iter()
                .find_map(|(written, call)| (*written == callee).then_some(*call))
        };
        let mut only = calls
            .iter()
            .map(|(callee, call)| (yielded(*callee), vec![*call]))
            .filter(|(expr, _)| names_function(*expr))
            .collect::<Vec<_>>();
        for (_, stmt) in body.stmts(db).iter() {
            let Partial::Present(Stmt::Let(pat, _, Some(init), None)) = stmt else {
                continue;
            };
            let value = yielded(*init);
            if !names_function(value) {
                continue;
            }
            let Some(binding) = self.pat_binding(*pat) else {
                continue;
            };
            let uses = body
                .exprs(db)
                .keys()
                .filter(|expr| {
                    self.typed_expr(*expr)
                        .is_some_and(|prop| prop.binding == Some(binding))
                })
                .collect::<Vec<_>>();
            let reached = uses
                .iter()
                .map(|use_| call_of(*use_))
                .collect::<Option<Vec<_>>>();
            if let Some(reached) = reached
                && !reached.is_empty()
            {
                only.push((value, reached));
            }
        }
        only
    }

    /// The choices the use at `expr` reads: a call's own and those it
    /// delivers, a constant use's own and those it delivers.
    fn choices_read_at(&self, expr: ExprId) -> Vec<Choice<'db>> {
        let db = self.db();
        let mut read: Vec<Choice<'db>> = Vec::new();
        if let Some(SemanticExprLowering::Call { callable }) =
            self.semantic_expr_lowering[expr].as_ref()
        {
            read.extend(callable.choice());
            read.extend(callable.delivered().iter().map(|(_, choice)| *choice));
        }
        let delivered = match self.const_refs[expr] {
            Some(ConstRef::TraitConst(use_)) => {
                read.extend(use_.choice());
                use_.delivered()
            }
            Some(ConstRef::InherentConst(use_)) => use_.delivered(),
            _ => None,
        };
        if let Some(list) = delivered {
            read.extend(list.items(db).iter().map(|(_, choice)| *choice));
        }
        read
    }

    /// A trait constant use with its decisions: its own slot's, and one for
    /// each bound of the constant body that runs (an implementation's bounds,
    /// a default body's `Self` and supertraits), as for a call. `Err` as for
    /// a call (`call_decisions`).
    pub(super) fn trait_const_decisions(
        &self,
        frame: Option<FrameId>,
        use_: crate::analysis::ty::assoc_const::AssocConstUse<'db>,
        reached: &mut Vec<TraitInstId<'db>>,
    ) -> Result<crate::analysis::ty::assoc_const::AssocConstUse<'db>, ChoiceRefusal> {
        let db = self.db();
        let own = self.decide(frame, use_.inst());
        let use_ = use_.with_choice(own);
        let mut undecided = Vec::new();
        let items = match use_.body_that_runs(db) {
            Some(runs) => self.body_decisions(frame, runs, false, &mut undecided, reached)?,
            // A body chosen later: the trait's default body declares the
            // slots a constant's declaration has (its `Self` and supertraits),
            // reached by name (`DeliveryList::to_body`), and each
            // implementation that applies to the goal as written has its
            // constant's bounds decided here, as a call's candidates are.
            None => {
                let mut items = match use_.trait_default_body(db) {
                    Some(default) => {
                        self.body_decisions(frame, default, true, &mut undecided, reached)?
                    }
                    None => Vec::new(),
                };
                let candidates = self
                    .candidate_slot(use_.inst())
                    .map_or_else(Vec::new, |inst| {
                        crate::analysis::ty::trait_def::candidate_constants(
                            db,
                            self.scope().top_mod(db).ingot(db),
                            inst,
                            use_.name(),
                        )
                    });
                for candidate in candidates {
                    for item in
                        self.body_decisions(frame, candidate, false, &mut undecided, reached)?
                    {
                        if !items.contains(&item) {
                            items.push(item);
                        }
                    }
                }
                items
            }
        };
        if let Some(refusal) =
            self.reads_opened_condition(frame, undecided, Some(slot_goal(db, use_.inst())))
        {
            return Err(refusal);
        }
        Ok(use_.with_delivered((!items.is_empty()).then(|| DeliveryList::new(db, items))))
    }

    /// An inherent constant use with the decisions for its implementation's
    /// bounds.
    pub(super) fn inherent_const_decisions(
        &self,
        frame: Option<FrameId>,
        use_: crate::analysis::ty::assoc_const::InherentConstUse<'db>,
        reached: &mut Vec<TraitInstId<'db>>,
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
        let Some(ty) =
            crate::analysis::ty::const_ty::inherent_const_decl_ty(db, use_.impl_(), use_.name())
        else {
            return Ok(use_);
        };
        let mut undecided = Vec::new();
        let items =
            self.body_decisions(frame, (body, ty, subst), false, &mut undecided, reached)?;
        if let Some(refusal) = self.reads_opened_condition(frame, undecided, None) {
            return Err(refusal);
        }
        Ok(use_.with_delivered((!items.is_empty()).then(|| DeliveryList::new(db, items))))
    }

    /// The decisions for a constant body's bounds, by its own names; the
    /// goals left ordinary are added to `undecided`. A bound whose choice
    /// reaches a type the body reads (`const_type_reads`) receives no opened
    /// or evidence choice (refused as a call into a function's is); one
    /// that passes the user's own bound on is added to `reached`, which the
    /// user owes its callers (`own_type_reads`).
    fn body_decisions(
        &self,
        frame: Option<FrameId>,
        (body, ty, subst): (
            crate::hir_def::Body<'db>,
            TyId<'db>,
            crate::analysis::ty::ty_lower::CompleteSubst<'db>,
        ),
        nominal: bool,
        undecided: &mut Vec<TraitInstId<'db>>,
        reached: &mut Vec<TraitInstId<'db>>,
    ) -> Result<Vec<(Delivery<'db>, Choice<'db>)>, ChoiceRefusal> {
        let db = self.db();
        let generic = !self.own_bounds().is_empty();
        if !has_frames(frame) && !generic {
            return Ok(Vec::new());
        }
        let mut items = Vec::new();
        for (bound, at_use) in const_declared_goals(db, body, &subst) {
            let Some(choice) = self.decide(frame, at_use) else {
                undecided.push(at_use);
                continue;
            };
            if const_type_reads(db, body, ty).contains(&bound) {
                match choice {
                    Choice::Slot(SlotRef::Bound(goal)) => {
                        if !reached.contains(&goal) {
                            reached.push(goal);
                        }
                    }
                    // Refused where it is read (`written_frames`).
                    Choice::Slot(SlotRef::Unsupplied) => {}
                    _ => return Err(ChoiceRefusal::DependentType),
                }
            }
            let delivery = if nominal {
                Delivery::Nominal(bound)
            } else {
                Delivery::Bound(bound)
            };
            items.push((delivery, choice));
        }
        Ok(items)
    }
}

/// Every slot whose choice reaches a type a checked body writes or names:
/// its expressions' and patterns' types, the constants written in it, each
/// recorded call's declared types and generic arguments (`call_slot_reads`,
/// the list the call refusal reads, so a caller and the body read one list),
/// and the slots its constant uses pass on to a constant body whose types
/// read them. Type reach of function bodies (`own_type_reads`) and constant
/// bodies (`const_type_reads`) reads only this list.
fn body_type_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &super::TypedBody<'db>,
) -> Vec<TraitInstId<'db>> {
    let mut reads = typed.const_reaches().to_vec();
    // The types a constant use is written over, which no expression's type
    // shows: an inherent constant's receiver (`Arr<<T as Width>::N>::K`), a
    // trait constant's goal.
    for cref in typed.const_refs() {
        match cref {
            ConstRef::InherentConst(use_) => {
                reads.extend(type_slot_reads(db, use_.receiver_ty()));
            }
            ConstRef::TraitConst(use_) => {
                for ty in use_.inst().args(db) {
                    reads.extend(type_slot_reads(db, *ty));
                }
            }
            ConstRef::Const(_) => {}
        }
    }
    let Some(body) = typed.body() else {
        return reads;
    };
    for expr in body.exprs(db).keys() {
        reads.extend(type_slot_reads(db, typed.expr_ty(db, expr)));
    }
    for (_, callable, _) in typed.recorded_calls(db) {
        reads.extend(call_slot_reads(db, callable));
    }
    for pat in body.pats(db).keys() {
        reads.extend(type_slot_reads(db, typed.pat_ty(db, pat)));
    }
    // Every constant written in the body, in any position: lowering reads it
    // with the ordinary implementation in the instance (`Arr<{ T::P }>::K`
    // has type `usize`, so no expression type shows the read).
    for written in super::written_frames::written_bodies(db, body) {
        reads.extend(
            super::written_frames::written_body_uses(db, written, None)
                .into_iter()
                .map(|(goal, _)| goal),
        );
    }
    let mut unique = Vec::new();
    for goal in reads {
        let goal = slot_goal(db, goal);
        if !unique.contains(&goal) {
            unique.push(goal);
        }
    }
    unique
}

/// The bounds of a constant body, by its own names, whose choices reach a
/// type the body writes or names (`body_type_reads`), directly or through a
/// function it calls. A use may not deliver an opened choice into them
/// (`body_decisions`), as a caller may not into a function's
/// (`own_type_reads`).
#[salsa::tracked(return_ref, cycle_initial = const_type_reads_initial, cycle_fn = const_type_reads_recover)]
pub(crate) fn const_type_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    expected: TyId<'db>,
) -> Vec<TraitInstId<'db>> {
    let own = crate::analysis::ty::trait_choice::const_body_slots(db, body);
    if own.is_empty() {
        return Vec::new();
    }
    let typed = &super::infer_body(db, super::BodyOwner::AnonConstBody { body, expected }).1;
    let mut reads = body_type_reads(db, typed);
    if let Some(checked) = typed.body() {
        for (_, callable, direct) in typed.recorded_calls(db) {
            reads.extend(super::const_requirements::reached_bounds_at_call(
                db,
                typed,
                checked.scope(),
                callable,
                direct,
            ));
        }
    }
    let mut reached = Vec::new();
    for goal in reads {
        if own.contains(&goal) && !reached.contains(&goal) {
            reached.push(goal);
        }
    }
    reached
}

fn const_type_reads_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _body: crate::hir_def::Body<'db>,
    _expected: TyId<'db>,
) -> Vec<TraitInstId<'db>> {
    Vec::new()
}

fn const_type_reads_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Vec<TraitInstId<'db>>,
    _count: u32,
    _body: crate::hir_def::Body<'db>,
    _expected: TyId<'db>,
) -> salsa::CycleRecoveryAction<Vec<TraitInstId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

/// The bound slots of `func` whose choices reach a type `func` itself
/// writes or names: in its body (`body_type_reads`), in its signature
/// (marked), and in a function value that would carry the slot's choice. A
/// caller may not deliver an opened choice into them (`OwedCheck::Reaches`);
/// slots reached through callees are added by `owed_checks`.
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
    for goal in declared_slot_reads(db, CallableDef::Func(func)) {
        note(goal, true);
    }
    for goal in body_type_reads(db, typed) {
        note(goal, false);
    }
    for (_, captured) in typed
        .function_value_choices()
        .iter()
        .chain(typed.closure_choices())
    {
        for choice in captured {
            if let Choice::Slot(SlotRef::Bound(goal)) = choice {
                note(*goal, false);
            }
        }
    }
    reached
}

/// A constant refused where it is read (`written_frames`) stays in the types
/// a body records: a trait constant read unsupplied, or a constant body left
/// unevaluated. When no other check of the body reported the refusal, it is
/// reported here, once, so no refused read passes silently.
pub(super) fn unreported_choice_dependent_consts<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &super::TypedBody<'db>,
) -> Option<crate::analysis::ty::diagnostics::FuncBodyDiag<'db>> {
    struct Finds<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Finds<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            if super::written_frames::refused_at_read(self.db, *const_ty) {
                self.found = true;
            }
            walk_const_ty(self, const_ty)
        }
    }
    let body = typed.body()?;
    let mut finds = Finds { db, found: false };
    typed.visit_with(&mut finds);
    finds.found.then(|| {
        BodyDiag::ChoiceDependentType {
            primary: body.expr(db).span(body).into(),
        }
        .into()
    })
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
        let primary = expr.span(body).into();
        diags.push(match refusal {
            ChoiceRefusal::OpenedCondition => BodyDiag::ObligationOpening {
                primary,
                reason: "a bound of this call is answered by an implementation with a condition on a slot opened here; conditional implementations under `with` are not supported yet",
            }
            .into(),
            ChoiceRefusal::UnfollowedProof => BodyDiag::ObligationOpening {
                primary,
                reason: "a bound of this call has a proof that cannot be followed to one implementation while a slot is opened here, so it is not known to avoid that slot; this is not supported yet",
            }
            .into(),
            ChoiceRefusal::DependentType => BodyDiag::ChoiceDependentType { primary }.into(),
            ChoiceRefusal::UninferredEvidence => BodyDiag::ObligationOpening {
                primary,
                reason: "this `with` operand is evidence whose type is known only after inference; write its type where it is bound",
            }
            .into(),
            ChoiceRefusal::ClosureChoiceAndProvider => BodyDiag::ObligationOpening {
                primary,
                reason: "a closure that reads a `with` choice and needs an effect provider is not supported yet",
            }
            .into(),
        });
    }
    for (expr, read) in typed.closure_choices() {
        if read.iter().any(|choice| !choice.is_ordinary_or_bound()) {
            diags.push(
                BodyDiag::ObligationOpening {
                    primary: expr.span(body).into(),
                    reason: "a closure cannot read an evidence choice of its enclosing function yet",
                }
                .into(),
            );
        }
    }
    for (expr, captured) in typed.function_value_choices() {
        if captured.iter().any(|choice| !choice.is_ordinary_or_bound()) {
            diags.push(
                BodyDiag::ChoiceCarried {
                    primary: expr.span(body).into(),
                    reason: "a function value written here would carry the `with` choice to where it is called, which is not supported yet; call it here",
                }
                .into(),
            );
        }
    }
    let mut refused_calls = Vec::new();
    for (expr, callable, direct) in typed.recorded_calls(db) {
        use super::const_requirements::{EdgeRefusal, call_refusal};
        let Some(refusal) = call_refusal(db, typed, scope, callable, direct) else {
            continue;
        };
        let key = (expr, std::mem::discriminant(&refusal));
        if refused_calls.contains(&key) {
            continue;
        }
        refused_calls.push(key);
        let primary = expr.span(body).into();
        match refusal {
            // A call refused for the types it declares is that refusal.
            EdgeRefusal::TypeReach
                if typed.choice_refusals().iter().any(|(refused, refusal)| {
                    *refused == expr && matches!(refusal, ChoiceRefusal::DependentType)
                }) => {}
            EdgeRefusal::TypeReach => diags.push(
                BodyDiag::ChoiceCarried {
                    primary,
                    reason: "this call passes a `with` choice into a body that reads it in a type, a function value or a closure, which is not supported yet",
                }
                .into(),
            ),
            EdgeRefusal::EvidenceThroughParam => diags.push(
                BodyDiag::ObligationOpening {
                    primary,
                    reason: "evidence passed as a value of a type parameter reaches a body that opens it or passes it on where no choice arrives with it; pass it where its `Evidence` type is written",
                }
                .into(),
            ),
            EdgeRefusal::OpenedCondition => diags.push(
                BodyDiag::ObligationOpening {
                    primary,
                    reason: "a body this call runs leaves a bound to an implementation with a condition on a slot this call delivers an opened choice to; conditional implementations under `with` are not supported yet",
                }
                .into(),
            ),
            EdgeRefusal::TooDeep => diags.push(
                BodyDiag::ObligationOpening {
                    primary,
                    reason: "the checks this call owes through bodies chosen later could not be followed to an end",
                }
                .into(),
            ),
        }
    }
    diags
}
