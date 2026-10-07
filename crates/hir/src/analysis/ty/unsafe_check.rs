use crate::analysis::HirAnalysisDb;
use crate::analysis::place::{PlaceBase, resolve_place_field};
use crate::analysis::semantic::capability::semantics::field_handle_spaces;
use crate::analysis::semantic::effect_param_site;
use crate::analysis::ty::corelib::{effect_key_state_access, resolve_core_trait};
use crate::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use crate::analysis::ty::effects::rows::expand_rows;
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::provider::{
    EffectHandleResolution, ProviderAddressSpace, provider_semantics, resolve_effect_handle,
};
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::{TraitSolveCx, is_goal_satisfiable};
use crate::analysis::ty::ty_check::{BindingAccess, BodyOwner, LocalBinding, TypedBody};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::ty_is_copy;
use crate::analysis::ty::ty_lower::collect_layout_arg_bindings;
use crate::core::semantic::EffectEnvView;
use crate::hir_def::{
    BlockKind, Body, CallableDef, Cond, CondId, Expr, ExprId, Partial, Stmt, StmtId, UnOp,
};

/// Reports the unsafe operations of a body that are outside an unsafe
/// context: an `unsafe { .. }` block, or the body of an `unsafe fn`. These are
/// dereferencing a raw pointer, explicitly or by selecting a field through
/// it, calling an `unsafe fn`, and binding a raw-pointer effect provider.
///
/// Nested items and anonymous const bodies are checked as their own owners,
/// and never inherit an enclosing unsafe context.
pub(crate) fn check_unsafe_ops<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    typed_body: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let Some(body) = owner.body(db) else {
        return Vec::new();
    };
    if let BodyOwner::Func(func) = owner
        && func.is_unsafe(db)
    {
        return Vec::new();
    }

    let mut checker = UnsafeChecker {
        db,
        owner,
        body,
        typed_body,
        diags: Vec::new(),
    };
    checker.check_expr(body.expr(db));
    checker.diags
}

/// Walks the safe code of a body; it never enters an `unsafe` block, since
/// everything there is allowed.
struct UnsafeChecker<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> UnsafeChecker<'db, '_> {
    fn report_deref(&mut self, expr: ExprId) {
        self.diags.push(
            BodyDiag::UnsafeDerefRequiresUnsafe {
                primary: expr.span(self.body).into(),
            }
            .into(),
        );
    }

    /// Reports a call, method call or operator whose callee is an
    /// `unsafe fn`. A trait method call resolves to the trait's declaration,
    /// whose unsafety every implementation shares.
    fn check_call_target(&mut self, expr: ExprId) {
        if let Some(callable) = self.typed_body.callable_expr(expr)
            && let callee @ CallableDef::Func(func) = callable.callable_def()
            && func.is_unsafe(self.db)
        {
            self.diags.push(
                BodyDiag::UnsafeCallRequiresUnsafe {
                    primary: expr.span(self.body).into(),
                    callee,
                }
                .into(),
            );
        }
    }

    /// Reports a `with` binding whose provider is an effect handle that
    /// provides its target through a raw pointer. Only the trusted handles of
    /// `std` carry a numeric `Raw`; any other handle, including a generic one,
    /// may give its effect access to memory nothing has checked. A handle that
    /// cannot provide its target is only a value, and binding it is safe.
    fn check_provider(&mut self, value: ExprId) {
        let ty = self.typed_body.expr_ty(self.db, value);
        let provider_ty = ty.as_view(self.db).unwrap_or(ty);
        let scope = self.body.scope();
        let assumptions = self.typed_body.assumptions();
        if let EffectHandleResolution::Resolved {
            target_ty, raw_ty, ..
        } = resolve_effect_handle(self.db, scope, assumptions, provider_ty)
            && raw_ty != TyId::u256(self.db)
            && let Some(effect_ref) = resolve_core_trait(self.db, scope, &["EffectRef"])
            && is_goal_satisfiable(
                self.db,
                TraitSolveCx::new(self.db, scope).with_assumptions(assumptions),
                TraitInstId::new_simple(self.db, effect_ref, vec![provider_ty, target_ty]),
            )
            .is_satisfied()
        {
            self.diags.push(
                BodyDiag::UnsafeProviderRequiresUnsafe {
                    primary: value.span(self.body).into(),
                }
                .into(),
            );
        }
    }

    /// Reports a `with` binding whose provider names existing storage or
    /// transient state that the function's own effects do not cover, so that
    /// nonlocal interference never disappears from its contract. A place in
    /// one of its effects, or in an access, carries that authority; a copied
    /// handle needs an effect of its type (`Field(T)`) or raw state authority.
    fn check_provider_coverage(&mut self, value: ExprId) {
        let scope = self.body.scope();
        let assumptions = self.typed_body.assumptions();
        let ty = normalize_ty(
            self.db,
            self.typed_body.expr_ty(self.db, value),
            scope,
            assumptions,
        );
        let names_resource = matches!(
            provider_semantics(self.db, scope, assumptions, ty).address_space,
            Some(ProviderAddressSpace::Storage | ProviderAddressSpace::Transient)
        ) || !field_handle_spaces(self.db, scope, assumptions, ty).is_empty();
        let authority = self.typed_body.expr_place(value).is_some_and(|place| {
            let PlaceBase::Binding(binding) = place.base;
            match (binding, self.typed_body.binding_access(binding)) {
                (LocalBinding::EffectParam { .. }, _) => true,
                (_, Some(BindingAccess::View)) => !ty_is_copy(
                    self.db,
                    scope,
                    self.typed_body.binding_ty(self.db, binding),
                    assumptions,
                ),
                (_, access) => access.is_some(),
            }
        });
        if authority || !names_resource {
            return;
        }
        let is_mut = self.typed_body.expr_prop(self.db, value).is_mut;
        let requirements = effect_param_site(self.owner)
            .map(|site| EffectEnvView::new(site).requirements(self.db))
            .unwrap_or_default();
        let rows = expand_rows(self.db, &requirements, scope, assumptions);
        let covered = requirements
            .iter()
            .chain(
                rows.components
                    .iter()
                    .map(|component| &component.requirement),
            )
            .filter(|requirement| requirement.is_mut || !is_mut)
            .any(|requirement| {
                requirement.key.key_ty().is_some_and(|key| {
                    let key = normalize_ty(self.db, key, scope, assumptions);
                    collect_layout_arg_bindings(self.db, key, ty, &mut Vec::new())
                }) || effect_key_state_access(
                    self.db,
                    scope,
                    assumptions,
                    requirement.key.clone(),
                    requirement.is_mut,
                )
                .is_some()
            });
        if !covered {
            self.diags.push(
                BodyDiag::UncoveredProvider {
                    primary: value.span(self.body).into(),
                    is_mut,
                }
                .into(),
            );
        }
    }

    fn check_stmt(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            return;
        };

        match stmt_data {
            Stmt::Let(_, _, init, else_) => {
                for expr in init.iter().chain(else_) {
                    self.check_expr(*expr);
                }
            }
            Stmt::For(_, iter, driver, body, _) => {
                for expr in [*iter].into_iter().chain(*driver).chain([*body]) {
                    self.check_expr(expr);
                }
            }
            Stmt::While(cond, body) => {
                self.check_cond(*cond);
                self.check_expr(*body);
            }
            Stmt::Continue | Stmt::Break => {}
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(*expr);
                }
            }
            Stmt::Yield(expr) | Stmt::Expr(expr) => self.check_expr(*expr),
        }
    }

    fn check_expr(&mut self, expr: ExprId) {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return;
        };

        match expr_data {
            Expr::Lit(_) | Expr::Path(_) | Expr::UnsupportedMacroCall => {}
            Expr::Block(_, BlockKind::Unsafe) => {}
            Expr::Block(stmts, BlockKind::Normal) => {
                stmts.iter().for_each(|stmt| self.check_stmt(*stmt));
            }
            Expr::Closure { body, .. } => self.check_expr(*body),

            Expr::Un(inner, op) => {
                self.check_expr(*inner);
                if *op == UnOp::Deref {
                    let ty = self.typed_body.expr_ty(self.db, *inner);
                    if ty
                        .as_capability(self.db)
                        .map_or(ty, |(_, inner)| inner)
                        .as_ptr(self.db)
                        .is_some()
                    {
                        self.report_deref(expr);
                    }
                }
                self.check_call_target(expr);
            }

            Expr::Field(inner, field) => {
                self.check_expr(*inner);
                if let Some(field) = field.to_opt()
                    && resolve_place_field(self.db, self.typed_body.expr_ty(self.db, *inner), field)
                        .is_some_and(|resolved| resolved.implicit_deref_ty.is_some())
                {
                    self.report_deref(expr);
                }
            }

            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.check_call_target(expr);
            }

            Expr::Cast(inner, _) | Expr::ArrayRep(inner, _) | Expr::Try(inner) => {
                self.check_expr(*inner)
            }

            Expr::Call(callee, args) => {
                self.check_expr(*callee);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.check_expr(*receiver);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call_target(expr);
            }
            Expr::Assert(args) => args.iter().for_each(|arg| self.check_expr(arg.expr)),

            Expr::If(cond, then, else_) => {
                self.check_cond(*cond);
                self.check_expr(*then);
                if let Some(else_) = else_ {
                    self.check_expr(*else_);
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.check_expr(*scrutinee);
                if let Partial::Present(arms) = arms {
                    arms.iter().for_each(|arm| self.check_expr(arm.body));
                }
            }
            Expr::With(bindings, body) => {
                for binding in bindings {
                    self.check_expr(binding.value);
                    self.check_provider(binding.value);
                    self.check_provider_coverage(binding.value);
                }
                self.check_expr(*body);
            }
            Expr::RecordInit(_, fields) => {
                fields.iter().for_each(|field| self.check_expr(field.expr));
            }
            Expr::Tuple(elems) | Expr::Array(elems) => {
                elems.iter().for_each(|elem| self.check_expr(*elem));
            }
        }
    }

    fn check_cond(&mut self, cond: CondId) {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            return;
        };

        match cond_data {
            Cond::Expr(expr) | Cond::Let(_, expr) => self.check_expr(*expr),
            Cond::Bin(lhs, rhs, _) => {
                self.check_cond(*lhs);
                self.check_cond(*rhs);
            }
        }
    }
}
