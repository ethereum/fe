mod callable;
mod env;
mod expr;
mod method_selection;
mod pat;
mod path;
mod stmt;

pub use callable::Callable;
pub use env::ExprProp;
use env::TyCheckEnv;
pub(super) use expr::TraitOps;
use hir::{
    hir_def::{Body, Expr, ExprId, Func, LitKind, Pat, PatId, TypeId as HirTyId},
    span::{expr::LazyExprSpan, pat::LazyPatSpan, DynLazySpan},
    visitor::{walk_expr, walk_pat, Visitor, VisitorCtxt},
};
pub(super) use path::RecordLike;
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    diagnostics::{BodyDiag, FuncBodyDiag, FuncBodyDiagAccumulator, TyDiagCollection, TyLowerDiag},
    fold::{TyFoldable, TyFolder},
    trait_def::{TraitInstId, TraitMethod},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, Kind, TyId, TyVarSort},
    ty_lower::lower_hir_ty,
    unify::{InferenceKey, UnificationError, UnificationTable},
};
use crate::{
    ty::ty_def::{inference_keys, TyFlags},
    HirAnalysisDb,
};

#[salsa::tracked(return_ref)]
pub fn check_func_body(db: &dyn HirAnalysisDb, func: Func) -> TypedBody {
    let Ok(mut checker) = TyChecker::new_with_func(db, func) else {
        return TypedBody::empty();
    };

    checker.run();
    checker.finish()
}

pub struct TyChecker<'db> {
    db: &'db dyn HirAnalysisDb,
    env: TyCheckEnv<'db>,
    table: UnificationTable<'db>,
    expected: TyId,
}

impl<'db> TyChecker<'db> {
    fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func) -> Result<Self, ()> {
        let env = TyCheckEnv::new_with_func(db, func)?;
        let expected_ty = match func.ret_ty(db.as_hir_db()) {
            Some(hir_ty) => {
                let ty = lower_hir_ty(db, hir_ty, func.scope());
                if ty.is_star_kind(db) {
                    ty
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                }
            }
            None => TyId::unit(db),
        };

        Ok(Self::new(db, env, expected_ty))
    }

    fn run(&mut self) {
        let root_expr = self.env.body().expr(self.db.as_hir_db());
        self.check_expr(root_expr, self.expected);
    }

    fn finish(self) -> TypedBody {
        TyCheckerFinalizer::new(self).finish()
    }

    fn new(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
        }
    }

    fn body(&self) -> Body {
        self.env.body()
    }

    fn lit_ty(&mut self, lit: &LitKind) -> TyId {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db.as_hir_db());
                self.table
                    .new_var(TyVarSort::String(len_bytes), &Kind::Star)
            }
        }
    }

    fn lower_ty(&self, hir_ty: HirTyId, span: DynLazySpan, star_kind_required: bool) -> TyId {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope());
        if let Some(diag) = ty.emit_diag(self.db, span.clone()) {
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        if star_kind_required && ty.is_star_kind(self.db) {
            ty
        } else {
            let diag: TyDiagCollection = TyLowerDiag::expected_star_kind_ty(span).into();
            FuncBodyDiagAccumulator::push(self.db, diag.into());
            TyId::invalid(self.db, InvalidCause::Other)
        }
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the sort is `General`.
    fn fresh_ty(&mut self) -> TyId {
        self.table.new_var(TyVarSort::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId, expected: TyId) -> TyId
    where
        T: Into<Typeable>,
    {
        let t = t.into();
        let actual = self.equate_ty(actual, expected, t.lazy_span(self.env.body()));

        match t {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.swap_ty(actual);
                self.env.type_expr(expr, typed_expr)
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
    }

    fn equate_ty(&mut self, actual: TyId, expected: TyId, span: DynLazySpan) -> TyId {
        // FIXME: This is a temporary workaround, this should be removed when we
        // implement subtyping.
        if expected.is_never(self.db) && !actual.is_never(self.db) {
            let diag = BodyDiag::type_mismatch(self.db, span, expected, actual).into();
            FuncBodyDiagAccumulator::push(self.db, diag);
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match self.table.unify(actual, expected) {
            Ok(()) => {
                // FIXME: This is a temporary workaround, this should be removed when we
                // implement subtyping.
                let actual = actual.fold_with(&mut self.table);
                if actual.is_never(self.db) {
                    expected
                } else {
                    actual
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(&mut self.table);
                let expected = expected.fold_with(&mut self.table);
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::type_mismatch(self.db, span, expected, actual).into(),
                );
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                FuncBodyDiagAccumulator::push(self.db, BodyDiag::InfiniteOccurrence(span).into());

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody {
    body: Option<Body>,
    pat_ty: FxHashMap<PatId, TyId>,
    expr_ty: FxHashMap<ExprId, ExprProp>,
    callables: FxHashMap<ExprId, Callable>,
}

impl TypedBody {
    pub fn expr_ty(&self, db: &dyn HirAnalysisDb, expr: ExprId) -> TyId {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &dyn HirAnalysisDb, expr: ExprId) -> ExprProp {
        self.expr_ty
            .get(&expr)
            .copied()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn pat_ty(&self, db: &dyn HirAnalysisDb, pat: PatId) -> TyId {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable> {
        self.callables.get(&expr)
    }

    fn empty() -> Self {
        Self {
            body: None,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            callables: FxHashMap::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, derive_more::From)]
enum Typeable {
    Expr(ExprId, ExprProp),
    Pat(PatId),
}

impl Typeable {
    fn lazy_span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr, ..) => expr.lazy_span(body).into(),
            Self::Pat(pat) => pat.lazy_span(body).into(),
        }
    }
}

impl TraitMethod {
    fn instantiate_with_inst(
        self,
        table: &mut UnificationTable,
        receiver_ty: TyId,
        inst: TraitInstId,
    ) -> TyId {
        let db = table.db();
        let mut ty = TyId::func(db, self.0);

        for &arg in inst.args(db) {
            ty = TyId::app(db, ty, arg);
        }

        let inst_self = table.instantiate_to_term(inst.self_ty(db));
        table.unify(inst_self, receiver_ty).unwrap();

        table.instantiate_to_term(ty)
    }
}

struct TyCheckerFinalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    body: TypedBody,
    assumptions: PredicateListId,
    ty_vars: FxHashSet<InferenceKey>,
    diags: Vec<FuncBodyDiag>,
}

impl<'db> TyCheckerFinalizer<'db> {
    fn new(mut checker: TyChecker<'db>) -> Self {
        let assumptions = checker.env.assumptions();
        let (body, diags) = checker.env.finish(&mut checker.table);

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            diags,
        }
    }

    fn finish(mut self) -> TypedBody {
        self.check_unknown_types();

        for diag in self.diags {
            FuncBodyDiagAccumulator::push(self.db, diag);
        }
        self.body
    }

    fn check_unknown_types(&mut self) {
        impl<'db> Visitor for TyCheckerFinalizer<'db> {
            fn visit_pat(&mut self, ctxt: &mut VisitorCtxt<'_, LazyPatSpan>, pat: PatId, _: &Pat) {
                let ty = self.body.pat_ty(self.db, pat);
                let span = ctxt.span().unwrap();
                self.check_unknown(ty, span.clone().into());

                walk_pat(self, ctxt, pat)
            }

            fn visit_expr(
                &mut self,
                ctxt: &mut VisitorCtxt<'_, LazyExprSpan>,
                expr: ExprId,
                expr_data: &Expr,
            ) {
                // Skip the check if the expr is block.
                if !matches!(expr_data, Expr::Block(..)) {
                    let prop = self.body.expr_prop(self.db, expr);
                    let span = ctxt.span().unwrap();
                    self.check_unknown(prop.ty, span.clone().into());
                    if prop.binding.is_none() {
                        self.check_wf(prop.ty, span.into());
                    }
                }

                // We need this additional check for method call because the callable type is
                // not tied to the expression type.
                if let Expr::MethodCall(..) = expr_data {
                    if let Some(callable) = self.body.callable_expr(expr) {
                        let callable_ty = callable.ty(self.db);
                        let span = ctxt.span().unwrap().into_method_call_expr().method_name();
                        self.check_unknown(callable_ty, span.clone().into());
                        self.check_wf(callable_ty, span.into())
                    }
                }

                walk_expr(self, ctxt, expr);
            }

            fn visit_item(
                &mut self,
                _: &mut VisitorCtxt<'_, hir::visitor::prelude::LazyItemSpan>,
                _: hir::hir_def::ItemKind,
            ) {
            }
        }

        if let Some(body) = self.body.body {
            let mut ctxt = VisitorCtxt::with_body(self.db.as_hir_db(), body);
            self.visit_body(&mut ctxt, body);
        }
    }

    fn check_unknown(&mut self, ty: TyId, span: DynLazySpan) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || !flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let mut skip_diag = false;
        for key in inference_keys(self.db, &ty) {
            // If at least one of the inference keys are already seen, we will skip emitting
            // diagnostics.
            skip_diag |= !self.ty_vars.insert(key);
        }

        if !skip_diag {
            let diag = BodyDiag::type_annotation_needed(self.db, span, ty);
            self.diags.push(diag.into())
        }
    }

    fn check_wf(&mut self, ty: TyId, span: DynLazySpan) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        if let Some(diag) = ty.emit_wf_diag(self.db, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}
