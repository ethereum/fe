mod env;
pub(crate) mod pat;
mod stmt;

use env::TyCheckEnv;
use hir::{
    hir_def::{Body, Expr, ExprId, Func, LitKind, Partial, PatId, TypeId as HirTyId},
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{InvalidCause, Kind, TyId, TyVarUniverse},
    ty_lower::lower_hir_ty,
    unify::{UnificationError, UnificationTable},
};
use crate::HirAnalysisDb;

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
        self.check_expr_ty(root_expr, self.expected);
    }

    fn finish(mut self) -> TypedBody {
        // TODO: check for untyped expressions and patterns.
        self.env.finish(&mut self.table)
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

    fn lower_ty(&self, hir_ty: HirTyId, span: DynLazySpan) -> TyId {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope());
        if let Some(diag) = ty.emit_diag(self.db, span) {
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        ty
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the universe is `General`.
    fn fresh_ty(&mut self) -> TyId {
        self.table.new_var(TyVarUniverse::General, &Kind::Star)
    }

    fn check_expr_ty(&mut self, expr: ExprId, expected: TyId) -> TyId {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let ty = TyId::invalid(self.db, InvalidCause::Other);
            self.env.type_expr(expr, ty);
            return ty;
        };

        let actual = match expr_data {
            Expr::Lit(lit) => self.lit_ty(lit),

            Expr::Block(stmts) => {
                if stmts.is_empty() {
                    TyId::unit(self.db)
                } else {
                    self.env.enter_block(expr).ok();
                    for &stmt in stmts[..stmts.len() - 1].iter() {
                        self.check_stmt(stmt, TyId::bot(self.db));
                    }

                    let last_stmt = stmts[stmts.len() - 1];
                    let res = self.check_stmt(last_stmt, expected);
                    self.env.leave_block();
                    res
                }
            }

            Expr::Bin(..) => todo!(),
            Expr::Un(..) => todo!(),
            Expr::Call(..) => todo!(),
            Expr::MethodCall(..) => todo!(),
            Expr::Path(..) => todo!(),
            Expr::RecordInit(..) => todo!(),
            Expr::Field(..) => todo!(),
            Expr::Tuple(..) => todo!(),
            Expr::Index(..) => todo!(),
            Expr::Array(..) => todo!(),
            Expr::ArrayRep(..) => todo!(),
            Expr::If(..) => todo!(),
            Expr::Match(..) => todo!(),
            Expr::Assign(..) => todo!(),
            Expr::AugAssign(..) => todo!(),
        };

        self.unify_ty(expr, actual, expected)
    }

    fn lit_ty(&mut self, lit: &LitKind) -> TyId {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarUniverse::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db.as_hir_db());
                self.table
                    .new_var(TyVarUniverse::String(len_bytes), &Kind::Star)
            }
        }
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId, expected: TyId) -> TyId
    where
        T: Into<Typeable>,
    {
        let t = t.into();

        let actual = match self.table.unify(expected, actual) {
            Ok(()) => actual,

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.apply_subst(self.db, &mut self.table);
                let expected = expected.apply_subst(self.db, &mut self.table);
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::type_mismatch(
                        self.db,
                        t.lazy_span(self.env.body()),
                        expected,
                        actual,
                    )
                    .into(),
                );
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::InfiniteOccurrence(t.lazy_span(self.env.body())).into(),
                );

                TyId::invalid(self.db, InvalidCause::Other)
            }
        };

        match t {
            Typeable::Expr(expr) => self.env.type_expr(expr, actual),
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody {
    body: Option<Body>,
    pat_ty: FxHashMap<PatId, TyId>,
    expr_ty: FxHashMap<ExprId, TyId>,
}

impl TypedBody {
    pub fn expr_ty(&self, db: &dyn HirAnalysisDb, expr: ExprId) -> TyId {
        self.expr_ty
            .get(&expr)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn pat_ty(&self, db: &dyn HirAnalysisDb, pat: PatId) -> TyId {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    fn empty() -> Self {
        Self {
            body: None,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, derive_more::From)]
enum Typeable {
    Expr(ExprId),
    Pat(PatId),
}

impl Typeable {
    fn lazy_span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr) => expr.lazy_span(body).into(),
            Self::Pat(pat) => pat.lazy_span(body).into(),
        }
    }
}