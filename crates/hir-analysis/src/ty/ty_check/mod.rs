mod env;

use env::ThCheckEnv;
use hir::hir_def::{Body, Expr, ExprId, Func, LitKind, Partial, PatId, Stmt, StmtId};
use rustc_hash::FxHashMap;

use super::{
    diagnostics::{FuncBodyDiagAccumulator, TyCheckDiag},
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
    env: ThCheckEnv<'db>,
    table: UnificationTable<'db>,
    expected: TyId,
}

impl<'db> TyChecker<'db> {
    fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func) -> Result<Self, ()> {
        let env = ThCheckEnv::new_with_func(db, func)?;
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

    fn finish(self) -> TypedBody {
        self.env.finish()
    }

    fn new(db: &'db dyn HirAnalysisDb, env: ThCheckEnv<'db>, expected: TyId) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
        }
    }

    fn check_expr_ty(&mut self, expr: ExprId, expected: TyId) -> TyId {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let ty = TyId::invalid(self.db, InvalidCause::Other);
            self.env.type_expr(expr, ty);
            return ty;
        };

        let actual = match expr_data {
            Expr::Lit(lit) => match lit {
                LitKind::Bool(_) => TyId::bool(self.db),
                LitKind::Int(_) => self.table.new_var(TyVarUniverse::Integral, &Kind::Star),
                _ => todo!(),
            },

            Expr::Block(stmts) => {
                if stmts.is_empty() {
                    TyId::unit(self.db)
                } else {
                    self.env.enter_block(expr).ok();
                    for &stmt in stmts[..stmts.len() - 1].iter() {
                        self.check_stmt(stmt, TyId::bot(self.db));
                    }

                    let res = self.check_stmt(stmts[stmts.len() - 1], expected);
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
        };

        let actual = match self.table.unify(expected, actual) {
            Ok(()) => {
                let actual = actual.apply_subst(self.db, &mut self.table);
                self.env.type_expr(expr, actual);
                actual
            }
            Err(UnificationError::TypeMismatch) => {
                let actual = actual.apply_subst(self.db, &mut self.table);
                let expected = expected.apply_subst(self.db, &mut self.table);
                FuncBodyDiagAccumulator::push(
                    self.db,
                    TyCheckDiag::type_mismatch(
                        self.db,
                        expr.lazy_span(self.env.body()).into(),
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
                    TyCheckDiag::InfiniteOccurrence(expr.lazy_span(self.env.body()).into()).into(),
                );

                TyId::invalid(self.db, InvalidCause::Other)
            }
        };

        self.env.type_expr(expr, actual);
        actual
    }

    fn check_stmt(&mut self, stmt: StmtId, expected: TyId) -> TyId {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match stmt_data {
            Stmt::Let(..) => todo!(),
            Stmt::Assign(..) => todo!(),
            Stmt::For(..) => todo!(),
            Stmt::While(..) => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Break => todo!(),
            Stmt::Return(..) => todo!(),
            Stmt::Expr(expr) => self.check_expr_ty(*expr, expected),
        }
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
