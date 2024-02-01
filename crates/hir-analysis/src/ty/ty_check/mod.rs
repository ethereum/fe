mod env;

use std::ops::Range;

use env::ThCheckEnv;
use hir::{
    hir_def::{
        Body, Expr, ExprId, Func, IdentId, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{AdtDef, InvalidCause, Kind, TyId, TyVar, TyVarUniverse},
    ty_lower::lower_hir_ty,
    unify::{UnificationError, UnificationTable},
};
use crate::{name_resolution::resolve_path_early, HirAnalysisDb};

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
            Expr::Lit(lit) => self.lit_ty(lit),

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

        self.unify_ty(expr, actual, expected)
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

    fn check_pat(&mut self, pat: PatId, expected: TyId) -> TyId {
        let Partial::Present(pat_data) = pat.data(self.db.as_hir_db(), self.env.body()) else {
            let actual = TyId::invalid(self.db, InvalidCause::Other);
            return self.unify_ty(pat, actual, expected);
        };

        match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarUniverse::General, &Kind::Star);
                self.unify_ty(pat, ty_var, expected)
            }

            Pat::Rest => unreachable!(),

            Pat::Lit(lit) => {
                let actual = match lit {
                    Partial::Present(lit) => self.lit_ty(lit),
                    Partial::Absent => TyId::invalid(self.db, InvalidCause::Other),
                };
                self.unify_ty(pat, actual, expected)
            }

            Pat::Tuple(pat_tup) => {
                let (actual, rest_range) = self.unpack_rest_pat(pat_tup, expected, pat);
                let unified = self.unify_ty(pat, actual, expected);

                if unified.contains_invalid(self.db) {
                    pat_tup.iter().for_each(|&pat| {
                        self.env
                            .type_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
                    });
                    return unified;
                }

                for (i, &pat_ty) in unified.decompose_ty_app(self.db).1.iter().enumerate() {
                    if rest_range.contains(&i)
                        || pat_tup[i].is_rest(self.db.as_hir_db(), self.env.body())
                    {
                        continue;
                    }

                    self.check_pat(pat_tup[i], pat_ty);
                }

                unified
            }

            Pat::Path(path) => todo!(),

            Pat::PathTuple(path, tup) => todo!(),

            Pat::Record(path, fields) => todo!(),

            Pat::Or(lhs, rhs) => {
                self.check_pat(*lhs, expected);
                self.check_pat(*rhs, expected)
            }
        }
    }

    fn unpack_rest_pat(
        &mut self,
        pat_tup: &[PatId],
        expected: TyId,
        pat: PatId,
    ) -> (TyId, std::ops::Range<usize>) {
        let mut rest_start = None;
        for (i, &pat) in pat_tup.iter().enumerate() {
            if pat.is_rest(self.db.as_hir_db(), self.env.body()) && rest_start.replace(i).is_some()
            {
                let span = pat.lazy_span(self.env.body());
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::DuplicatedRestPat(span.into()).into(),
                );
                return (
                    TyId::invalid(self.db, InvalidCause::Other),
                    Range::default(),
                );
            }
        }

        let mut make_args = |len: usize| {
            (0..len)
                .map(|_| self.table.new_var(TyVarUniverse::General, &Kind::Star))
                .collect::<Vec<_>>()
        };

        match rest_start {
            Some(rest_start) => {
                let (base, expected_args) = expected.decompose_ty_app(self.db);
                let expected_args_len = expected_args.len();
                let minimum_len = expected_args.len() - 1;

                if base.is_tuple(self.db) && minimum_len <= expected_args.len() {
                    let diff = expected_args_len - minimum_len;
                    let range = rest_start..rest_start + diff;
                    let ty_args = make_args(expected_args_len);
                    (TyId::tuple_with_elems(self.db, &ty_args), range)
                } else {
                    let ty_args = make_args(minimum_len);
                    (TyId::tuple_with_elems(self.db, &ty_args), Range::default())
                }
            }

            None => {
                let ty_args = make_args(pat_tup.len());
                (TyId::tuple_with_elems(self.db, &ty_args), Range::default())
            }
        }
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
            Ok(()) => actual.apply_subst(self.db, &mut self.table),

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

#[derive(Clone)]
enum ResolvedPath {
    Adt(ResolvedAdt),
    NewVar(IdentId),
    NotFound,
}

#[derive(Clone)]
struct ResolvedAdt {
    adt: AdtDef,
    variant_idx: Option<usize>,
}
