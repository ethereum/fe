use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::method_selection::{MethodCandidate, select_method_candidate},
};
use crate::core::hir_def::{Expr, ExprId, IdentId, Partial, Pat, PatId, Stmt, StmtId, Trait, UnOp};
use crate::span::DynLazySpan;

use super::{
    Callable, LocalBinding, TyChecker,
    env::{ExprProp, TraitObligation, TraitObligationOrigin},
};
use crate::analysis::ty::{
    LayoutBundlePathStep,
    canonical::Canonicalized,
    corelib::resolve_core_trait,
    diagnostics::BodyDiag,
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_def::TraitInstId,
    trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
    ty_def::{BorrowKind, InvalidCause, TyId},
    visitor::TyVisitable,
};

/// A step of the protocol a `for` loop runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ForLoopStep {
    /// `start`: the first cursor or driver state.
    Start,
    /// `next`: the state after one.
    Next,
    /// `at`, or a producer's `produce`: the step's element.
    At,
}

impl ForLoopStep {
    pub const ALL: [Self; 3] = [Self::Start, Self::Next, Self::At];
}

/// What a loop's body receives from each step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub enum ForLoopItem {
    /// A copy of the element and of what a driver yields beside it, taken
    /// before the body runs, so the element's session ends first.
    Copy,
    /// The element's access, open while the body uses it.
    Access(BorrowKind),
    /// A producer's owned value.
    Produced,
}

/// A call of the loop protocol, resolved.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopCall<'db> {
    pub callable: Callable<'db>,
    pub effect_args: Vec<super::ResolvedEffectArg<'db>>,
}

/// How a `for` loop reaches its elements: the base it holds, the driver its
/// calls take first if it has one, the calls it selects and what its body
/// receives.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopPlan<'db> {
    /// The base the loop holds, or the two of a two-base driver.
    pub bases: Vec<ExprId>,
    /// The driver of `for .. by d`, or the method chain `for x in xs.reversed()`
    /// stands for `for x in xs by xs.reversed()`.
    pub driver: Option<ExprId>,
    pub state_ty: TyId<'db>,
    /// What an `at` or `produce` call yields: the element's access, beside
    /// what a driver yields with it, or a producer's value.
    pub shape: Shape<'db>,
    pub item: ForLoopItem,
    /// The pattern binds the element alone: a driver yields nothing beside it.
    pub binds_element: bool,
    /// The calls, in `ForLoopStep` order.
    pub calls: [ForLoopCall<'db>; 3],
    /// The element is the indexed layout projection of the base. Semantic
    /// lowering uses this explicit desugaring fact to retain the dynamic
    /// array-index source on the element.
    pub element_layout_backing_source: bool,
}

impl<'db> ForLoopPlan<'db> {
    pub fn call(&self, step: ForLoopStep) -> &ForLoopCall<'db> {
        &self.calls[step as usize]
    }

    /// The shape of what the pattern binds.
    pub fn pattern_shape(&self) -> &Shape<'db> {
        match &self.shape {
            Shape::Tuple(parts) if self.binds_element => &parts[1],
            shape => shape,
        }
    }
}

impl<'db> TyVisitable<'db> for ForLoopPlan<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.state_ty.visit_with(visitor);
        self.shape.visit_with(visitor);
        for call in &self.calls {
            call.callable.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for ForLoopPlan<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        ForLoopPlan {
            state_ty: self.state_ty.fold_with(db, folder),
            shape: self.shape.fold_with(db, folder),
            calls: self.calls.map(|call| ForLoopCall {
                callable: call.callable.fold_with(db, folder),
                effect_args: call.effect_args,
            }),
            ..self
        }
    }
}

impl<'db> TyChecker<'db> {
    pub(super) fn check_stmt(&mut self, stmt: StmtId, expected: TyId<'db>) -> TyId<'db> {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::ParseError);
        };

        match stmt_data {
            Stmt::Let(..) => self.check_let(stmt, stmt_data),
            Stmt::For(..) => self.check_for(stmt, stmt_data),
            Stmt::While(..) => self.check_while(stmt, stmt_data),
            Stmt::Continue => self.check_continue(stmt, stmt_data),
            Stmt::Break => self.check_break(stmt, stmt_data),
            Stmt::Return(..) => self.check_return(stmt, stmt_data),
            Stmt::Yield(expr) => self.check_yield(stmt, *expr),
            Stmt::Expr(expr) => self.check_expr(*expr, expected).ty,
        }
    }

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::Let(pat, ascription, expr, else_) = stmt_data else {
            unreachable!()
        };

        let span = stmt.span(self.env.body()).into_let_stmt();

        let ascription = ascription.map(|ty| self.lower_ty(ty, span.clone().ty(), true));

        if let Some(expr) = expr {
            let prop = if let Some(ascription) = ascription {
                self.check_expr(*expr, ascription)
            } else {
                self.check_expr_unknown(*expr)
            };
            let layout = self.pattern_layout_context(*expr);
            self.check_pat_with_layout(*pat, prop.ty, layout.as_ref());
            if let Some(LocalBinding::Local { pat, .. }) = self.env.pat_binding(*pat) {
                self.env
                    .set_local_borrow_provider(pat, prop.borrow_provider);
            }

            // `let mut x = p.get()` binds a mutable copy of a projection's
            // `Copy` grant (an explicit `mut p` stays an access).
            let copies_grant = matches!(
                pat.data(self.db, self.body()),
                Partial::Present(Pat::Path(_, true))
            ) && !matches!(
                self.env.expr_data(*expr),
                Partial::Present(Expr::Un(_, UnOp::Ref | UnOp::Mut))
            ) && matches!(prop.shape, Some(Shape::Access(_, ty)) if self.ty_is_copy(ty));
            if copies_grant {
                self.consume_access(*expr);
            } else if prop.shape.is_some() {
                self.bind_pattern_source(*pat, *expr, &prop);
            } else if self.pattern_binds_any(*pat) {
                self.record_implicit_move_for_owned_expr(*expr, prop.ty);
            }
            if let Some(else_) = else_ {
                let else_ty = self
                    .check_expr_unknown(*else_)
                    .ty
                    .fold_with(self.db, &mut self.table);
                if !else_ty.is_never(self.db) && !else_ty.has_invalid(self.db) {
                    self.push_diag(BodyDiag::LetElseMustDiverge {
                        primary: else_.span(self.body()).into(),
                    });
                }
            } else if let Some(root) = self.env.pattern_store().root(*pat)
                && !self.env.pattern_store().is_irrefutable(self.db, root)
            {
                self.push_diag(BodyDiag::RefutableLetPattern {
                    primary: pat.span(self.body()).into(),
                });
            }
        } else {
            let ascription = ascription.unwrap_or_else(|| self.fresh_ty());
            if let Some(diag) = ascription.emit_wf_diag(
                self.db,
                TraitSolveCx::new(self.db, self.env.scope()),
                self.env.assumptions(),
                span.ty().into(),
            ) {
                self.push_diag(diag);
            }
            self.check_pat(*pat, ascription);
        }
        self.check_mutable_pattern_bindings(*pat);
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }

    fn check_mutable_pattern_bindings(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(_, is_mut) => {
                if !*is_mut {
                    return;
                }

                let Some(binding) = self.env.pat_binding(pat) else {
                    return;
                };
                let ty = self.env.lookup_binding_ty(&binding);
                if ty.has_invalid(self.db) || self.env.binding_access(&binding).is_none() {
                    return;
                }

                self.push_diag(BodyDiag::MutableBindingCannotBeCapability {
                    primary: pat.span(self.body()).into_path_pat().mut_token().into(),
                    ty,
                });
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &pat in pats {
                    self.check_mutable_pattern_bindings(pat);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.check_mutable_pattern_bindings(field.pat);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.check_mutable_pattern_bindings(*lhs);
                self.check_mutable_pattern_bindings(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    fn check_for(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::For(pat, expr, driver, body, _unroll) = stmt_data else {
            unreachable!()
        };

        // A two-base driver traverses a pair of places, `for p in (a, b) by d`.
        let bases = match (driver, self.env.expr_data(*expr)) {
            (Some(_), Partial::Present(Expr::Tuple(elems))) if elems.len() == 2 => elems.clone(),
            _ => vec![*expr],
        };
        let mut checked = Vec::with_capacity(bases.len());
        let mut modes = Vec::with_capacity(bases.len());
        for &base in &bases {
            let expected = self.fresh_ty();
            let prop = self
                .check_expr(base, expected)
                .fold_with(self.db, &mut self.table);
            // The loop holds its base; `for pat in mut e` mutates the elements.
            if prop.shape.is_some() {
                self.consume_access(base);
            }
            modes.push(matches!(
                prop.shape,
                Some(Shape::Access(BorrowKind::Mut, _))
            ));
            checked.push((base, prop.ty));
        }
        if let [(_, a), (_, b)] = checked[..] {
            let pair = TyId::tuple_with_elems(self.db, &[a, b]);
            self.env.type_expr(*expr, ExprProp::new(pair, false));
            if modes[0] != modes[1] {
                self.push_diag(BodyDiag::MixedLoopBases {
                    primary: expr.span(self.body()).into(),
                });
                checked.clear();
            }
        }
        let mutates = modes.contains(&true);
        let driver = driver.map(|driver| {
            let driver_ty = self
                .check_expr_unknown(driver)
                .ty
                .fold_with(self.db, &mut self.table);
            (driver, driver_ty)
        });
        match self.plan_for_loop(&checked, driver, mutates) {
            Some(mut plan) => {
                // A producer advances its own copy of the driver.
                if let Some(driver) = plan.driver
                    && plan.item == ForLoopItem::Produced
                {
                    self.record_implicit_move_for_owned_expr_inner(driver, None);
                }
                let pattern_shape = plan.pattern_shape().clone();
                let layout = plan
                    .driver
                    .is_none()
                    .then(|| {
                        self.pattern_layout_context_for_projection(
                            *expr,
                            &[LayoutBundlePathStep::Index],
                        )
                    })
                    .flatten()
                    .filter(|layout| {
                        self.projected_pattern_layout_ty(layout, &[])
                            .is_some_and(|projected| {
                                crate::analysis::ty::layout_shape_key(self.db, projected)
                                    == crate::analysis::ty::layout_shape_key(
                                        self.db,
                                        pattern_shape.erased_ty(self.db),
                                    )
                            })
                    });
                plan.element_layout_backing_source = layout.is_some();
                self.check_pat_with_layout(*pat, pattern_shape.erased_ty(self.db), layout.as_ref());
                if let ForLoopItem::Access(_) = plan.item {
                    let authority = bases.iter().all(|base| self.expr_has_authority(*base));
                    self.bind_pattern_accesses(*pat, &pattern_shape, authority);
                }
                self.env.register_for_loop_plan(stmt, plan);
            }
            None => {
                self.check_pat(*pat, TyId::invalid(self.db, InvalidCause::Other));
            }
        }

        self.env.enter_loop(stmt);
        self.env.enter_scope(*body);
        self.env.flush_pending_bindings();

        let body_ty = self.fresh_ty();
        self.check_expr_with_discarded_result(*body, body_ty);

        self.env.leave_scope();
        self.env.leave_loop();

        TyId::unit(self.db)
    }

    /// The protocol a loop over `expr`, of type `ty`, runs: its driver's, or
    /// the collection's own. Without a driver, a method chain that drives a
    /// collection it passes through, `for x in xs.reversed()`, is a driver
    /// over it.
    fn plan_for_loop(
        &mut self,
        bases: &[(ExprId, TyId<'db>)],
        driver: Option<(ExprId, TyId<'db>)>,
        mutates: bool,
    ) -> Option<ForLoopPlan<'db>> {
        let &[(expr, ty), ..] = bases else {
            return None;
        };
        for &(expr, ty) in bases.iter().chain(&driver) {
            if ty.has_invalid(self.db) {
                return None;
            }
            if ty.is_never(self.db) || ty.base_ty(self.db).is_ty_var(self.db) {
                self.push_diag(BodyDiag::TypeMustBeKnown(expr.span(self.body()).into()));
                return None;
            }
        }
        let plan = match (driver, self.chain_base(expr, ty)) {
            (None, Some(base)) => self.plan_protocol(&[base], Some((expr, ty)), mutates),
            _ => self.plan_protocol(bases, driver, mutates),
        };
        if plan.is_none() {
            let (expr, ty, name) = match driver {
                Some((driver, driver_ty)) if bases.len() == 2 => (driver, driver_ty, "Driver2"),
                Some((driver, driver_ty)) => (driver, driver_ty, "Driver"),
                None => (expr, ty, "Collection"),
            };
            let name = if mutates {
                format!("{name}Mut")
            } else {
                name.to_string()
            };
            self.push_diag(BodyDiag::TraitNotImplemented {
                primary: expr.span(self.body()).into(),
                ty: ty.pretty_print(self.db).to_string(),
                trait_name: IdentId::new(self.db, name),
            });
        }
        plan
    }

    /// The receiver a method chain of type `ty` drives, the nearest first:
    /// `xs` in `xs.reversed().take(3)`, and the projected `buf.span()` in
    /// `buf.span().reversed()`.
    fn chain_base(&mut self, expr: ExprId, ty: TyId<'db>) -> Option<(ExprId, TyId<'db>)> {
        let traits = ["Driver", "Producer"]
            .map(|name| resolve_core_trait(self.db, self.env.scope(), &["iter", name]));
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        let mut receiver = expr;
        while let Partial::Present(Expr::MethodCall(next, ..)) = receiver.data(self.db, self.body())
        {
            receiver = *next;
            let receiver_ty = self
                .env
                .typed_expr(receiver)?
                .ty
                .fold_with(self.db, &mut self.table);
            let drives = traits.iter().flatten().any(|&trait_def| {
                let goal = TraitInstId::new_simple(self.db, trait_def, vec![ty, receiver_ty]);
                !matches!(
                    is_goal_satisfiable(self.db, solve_cx, goal),
                    GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                )
            });
            if drives {
                return Some((receiver, receiver_ty));
            }
        }
        None
    }

    /// The protocol of a loop over `base`, of type `base_ty`, run by `driver`
    /// or by the collection itself. A loop that mutates its elements selects
    /// the `mut` form of `at`; otherwise a producer's owned values come
    /// before a driver's elements, which the body sees as copies when they
    /// are `Copy` and as `ref` accesses otherwise.
    fn plan_protocol(
        &mut self,
        bases: &[(ExprId, TyId<'db>)],
        driver: Option<(ExprId, TyId<'db>)>,
        mutates: bool,
    ) -> Option<ForLoopPlan<'db>> {
        let span: DynLazySpan<'db> = bases[0].0.span(self.body()).into();
        let core_trait = |this: &Self, name: &str| {
            resolve_core_trait(this.db, this.env.scope(), &["iter", name])
        };
        let base_tys: Vec<_> = bases.iter().map(|&(_, ty)| ty).collect();
        // The calls take the driver first, then the bases.
        let (receiver_ty, inputs) = match driver {
            Some((_, driver_ty)) => (driver_ty, [&[driver_ty], &base_tys[..]].concat()),
            None => (base_tys[0], base_tys.clone()),
        };
        let select = |this: &mut Self, trait_def, method| {
            let base_tys = if driver.is_some() { &base_tys[..] } else { &[] };
            this.select_loop_trait(span.clone(), receiver_ty, trait_def, method, base_tys)
        };
        let (protocol, inst, at_trait, at_inst, at_name) = match driver {
            Some(_) if bases.len() == 2 => {
                let driver_trait = core_trait(self, "Driver2")?;
                let inst = select(self, driver_trait, "start")?;
                if mutates {
                    let driver_mut = core_trait(self, "DriverMut2")?;
                    let at_inst = select(self, driver_mut, "at_mut")?;
                    (driver_trait, inst, driver_mut, at_inst, "at_mut")
                } else {
                    (driver_trait, inst, driver_trait, inst, "at")
                }
            }
            None => {
                let collection = core_trait(self, "Collection")?;
                let inst = select(self, collection, "start")?;
                if mutates {
                    let collection_mut = core_trait(self, "CollectionMut")?;
                    let at_inst = select(self, collection_mut, "at_mut")?;
                    (collection, inst, collection_mut, at_inst, "at_mut")
                } else {
                    (collection, inst, collection, inst, "at")
                }
            }
            Some(_) => {
                let driver_trait = core_trait(self, "Driver")?;
                let producer = core_trait(self, "Producer")?;
                if mutates {
                    let driver_mut = core_trait(self, "DriverMut")?;
                    let at_inst = select(self, driver_mut, "at_mut")?;
                    let inst = select(self, driver_trait, "start")?;
                    (driver_trait, inst, driver_mut, at_inst, "at_mut")
                } else if let Some(inst) = select(self, producer, "produce") {
                    (producer, inst, producer, inst, "produce")
                } else {
                    let inst = select(self, driver_trait, "start")?;
                    (driver_trait, inst, driver_trait, inst, "at")
                }
            }
        };
        let call = |this: &mut Self, trait_def: Trait<'db>, inst, name: &str, inputs| {
            let method = *trait_def
                .method_defs(this.db)
                .get(&IdentId::new(this.db, name.to_string()))?;
            let func_ty = this.instantiate_trait_method_to_term(method, receiver_ty, inst);
            let mut callable = Callable::new(this.db, func_ty, span.clone(), Some(inst)).ok()?;
            callable.set_checked_input_tys(inputs);
            let args: Vec<_> = driver
                .map(|(driver, _)| driver)
                .into_iter()
                .chain(bases.iter().map(|&(base, _)| base))
                .collect();
            let effect_args = this.resolve_callable_effects(span.clone(), &mut callable, &args);
            Some(ForLoopCall {
                callable,
                effect_args,
            })
        };
        let start = call(self, protocol, inst, "start", inputs.clone())?;
        let state_ty = *self
            .normalize_ty(start.callable.ret_ty(self.db))
            .generic_args(self.db)
            .first()?;
        let with_state = || inputs.iter().copied().chain([state_ty]).collect::<Vec<_>>();
        let next = call(self, protocol, inst, "next", with_state())?;
        let at = call(self, at_trait, at_inst, at_name, with_state())?;
        let shape = match at.callable.ret_shape(self.db) {
            Some(shape) => shape.map_tys(&mut |ty| self.normalize_ty(ty)),
            None => Shape::Owned(self.normalize_ty(at.callable.ret_ty(self.db))),
        };
        // The elements: one beside what a driver yields with it, or one from
        // each base of a two-base driver.
        let elements: Vec<_> = match &shape {
            Shape::Tuple(parts) if bases.len() == 2 => {
                parts.iter().map(|part| part.erased_ty(self.db)).collect()
            }
            Shape::Tuple(parts) => parts
                .get(1)
                .map(|part| part.erased_ty(self.db))
                .into_iter()
                .collect(),
            Shape::Access(_, element) => vec![*element],
            _ => Vec::new(),
        };
        let item = if elements.is_empty() {
            ForLoopItem::Produced
        } else if mutates {
            ForLoopItem::Access(BorrowKind::Mut)
        } else if elements.iter().all(|&element| self.ty_is_copy(element)) {
            ForLoopItem::Copy
        } else {
            ForLoopItem::Access(BorrowKind::Ref)
        };
        // A driver that yields nothing beside its elements binds them alone.
        let binds_element = match &shape {
            Shape::Tuple(parts) => {
                bases.len() == 1 && parts[0] == Shape::Owned(TyId::unit(self.db))
            }
            _ => true,
        };
        Some(ForLoopPlan {
            bases: bases.iter().map(|&(base, _)| base).collect(),
            driver: driver.map(|(driver, _)| driver),
            state_ty,
            shape,
            item,
            binds_element,
            calls: [start, next, at],
            element_layout_backing_source: false,
        })
    }

    /// The instance of `trait_def`, selected through its method `method`, that
    /// a loop's receiver of type `receiver_ty` implements: for a driver, over
    /// the bases of types `base_tys`.
    fn select_loop_trait(
        &mut self,
        span: DynLazySpan<'db>,
        receiver_ty: TyId<'db>,
        trait_def: Trait<'db>,
        method: &str,
        base_tys: &[TyId<'db>],
    ) -> Option<TraitInstId<'db>> {
        let canonical = Canonicalized::new(self.db, receiver_ty);
        let (cand, confirm) = match select_method_candidate(
            self.db,
            &canonical,
            IdentId::new(self.db, method.to_string()),
            self.env.scope(),
            self.env.assumptions(),
            Some(trait_def),
        ) {
            Ok(MethodCandidate::TraitMethod(cand)) => (cand, false),
            Ok(MethodCandidate::NeedsConfirmation(cand)) => (cand, true),
            _ => return None,
        };
        let snapshot = self.snapshot_state();
        let inst = canonical.extract_solution(&mut self.table, cand.inst);
        if base_tys.iter().enumerate().any(|(idx, &base_ty)| {
            inst.args(self.db)
                .get(idx + 1)
                .is_none_or(|&arg| self.table.unify(arg, base_ty).is_err())
        }) {
            self.rollback_state(snapshot);
            return None;
        }
        self.commit_state(snapshot);
        let inst = inst.fold_with(self.db, &mut self.table);
        if confirm || !base_tys.is_empty() {
            self.env.register_trait_obligation(TraitObligation {
                goal: inst,
                origin: TraitObligationOrigin::GenericConfirmation,
                span,
            });
        }
        Some(inst)
    }

    fn check_while(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::While(cond, body) = stmt_data else {
            unreachable!()
        };

        // Keep let-chain bindings local to the loop condition/body.
        self.env.enter_lexical_scope();
        self.check_cond(*cond);

        self.env.enter_loop(stmt);
        self.env.enter_scope(*body);
        self.env.flush_pending_bindings();
        let body_ty = self.fresh_ty();
        self.check_expr_with_discarded_result(*body, body_ty);
        self.env.leave_scope();
        self.env.clear_pending_bindings();
        self.env.leave_loop();
        self.env.leave_scope();

        TyId::unit(self.db)
    }

    fn check_continue(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        assert!(matches!(stmt_data, Stmt::Continue));

        if self.env.current_loop().is_none() {
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: false,
            };
            self.push_diag(diag);
        }

        TyId::never(self.db)
    }

    fn check_break(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        assert!(matches!(stmt_data, Stmt::Break));

        if self.env.current_loop().is_none() {
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: true,
            };
            self.push_diag(diag);
        }

        TyId::never(self.db)
    }

    fn check_return(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::Return(expr) = stmt_data else {
            unreachable!()
        };
        // In a body with `yield` statements, a bare `return` ends the slide.
        if expr.is_some() || !self.explicit_yields || self.projection_shape.is_none() {
            self.check_exit_value(stmt, *expr);
        }
        TyId::never(self.db)
    }

    fn check_yield(&mut self, stmt: StmtId, expr: ExprId) -> TyId<'db> {
        if self.projection_shape.is_some() {
            self.check_exit_value(stmt, Some(expr));
        } else {
            self.check_expr(expr, self.expected);
            self.push_diag(BodyDiag::YieldOutsideProjection {
                primary: stmt.span(self.env.body()).into(),
            });
        }
        TyId::unit(self.db)
    }

    /// Checks the value a `return` or `yield` ends its path with.
    fn check_exit_value(&mut self, stmt: StmtId, expr: Option<ExprId>) {
        let (returned_expr, returned_prop, returned_ty, had_child_err) = if let Some(expr) = expr {
            let before = self.diags.len();
            let expected = self.fresh_ty();
            let prop = self.check_expr(expr, expected);
            let ty = expected.fold_with(self.db, &mut self.table);
            (Some(expr), Some(prop), ty, self.diags.len() > before)
        } else {
            (None, None, TyId::unit(self.db), false)
        };

        let ret_ty_ok = !had_child_err
            && !returned_ty.has_invalid(self.db)
            && self.table.unify(returned_ty, self.expected).is_ok();

        if !had_child_err && !returned_ty.has_invalid(self.db) && !ret_ty_ok {
            let func = self.env.func();
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::ReturnedTypeMismatch {
                primary: span.into(),
                actual: returned_ty,
                expected: self.expected,
                func,
            };

            self.push_diag(diag);
        } else if ret_ty_ok && let Some(expr) = returned_expr {
            match self.projection_shape.clone() {
                Some(shape) => self.check_yield_site(expr, &shape),
                None => self.record_implicit_move_for_owned_expr(expr, self.expected),
            }
        }

        if ret_ty_ok
            && returned_expr.is_some()
            && let Some(prop) = returned_prop
            && let Some(provider) = prop.borrow_provider
        {
            self.first_return_borrow_provider.get_or_insert(provider);
        }
    }
}
