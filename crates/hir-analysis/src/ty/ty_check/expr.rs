use hir::hir_def::{Expr, ExprId, Partial};

use super::path::ResolvedPathInExpr;
use crate::ty::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_check::{path::resolve_path_in_expr, TyChecker},
    ty_def::{InvalidCause, TyId},
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_expr(&mut self, expr: ExprId, expected: TyId) -> TyId {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let ty = TyId::invalid(self.db, InvalidCause::Other);
            self.env.type_expr(expr, ty);
            return ty;
        };

        let actual = match expr_data {
            Expr::Lit(lit) => self.lit_ty(lit),
            Expr::Block(..) => self.check_block(expr, expr_data, expected),

            Expr::Bin(..) => todo!(),
            Expr::Un(..) => todo!(),
            Expr::Call(..) => todo!(),
            Expr::MethodCall(..) => todo!(),
            Expr::Path(..) => self.check_path(expr, expr_data, expected),
            Expr::RecordInit(..) => todo!(),
            Expr::Field(..) => todo!(),
            Expr::Tuple(..) => todo!(),
            Expr::Index(..) => todo!(),
            Expr::Array(..) => todo!(),
            Expr::ArrayRep(..) => todo!(),
            Expr::If(..) => self.check_if(expr, expr_data, expected),
            Expr::Match(..) => todo!(),
            Expr::Assign(..) => todo!(),
            Expr::AugAssign(..) => todo!(),
        };

        self.unify_ty(expr, actual, expected)
    }

    fn check_block(&mut self, expr: ExprId, expr_data: &Expr, expected: TyId) -> TyId {
        let Expr::Block(stmts) = expr_data else {
            unreachable!()
        };

        if stmts.is_empty() {
            TyId::unit(self.db)
        } else {
            self.env.enter_scope(expr);
            for &stmt in stmts[..stmts.len() - 1].iter() {
                let ty = self.fresh_ty();
                self.check_stmt(stmt, ty);
            }

            let last_stmt = stmts[stmts.len() - 1];
            let res = self.check_stmt(last_stmt, expected);
            self.env.leave_scope();
            res
        }
    }

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr, _expected: TyId) -> TyId {
        let path = match expr_data {
            Expr::Path(path) => path,
            _ => unreachable!(),
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_expr(self, *path, expr) {
            ResolvedPathInExpr::Data(data) => {
                if data.is_unit_variant(self.db) {
                    data.ty(self.db)
                } else {
                    let diag = BodyDiag::unit_variant_expected(
                        self.db,
                        expr.lazy_span(self.body()).into(),
                        data,
                    )
                    .into();
                    FuncBodyDiagAccumulator::push(self.db, diag);

                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInExpr::Func(func) => func.ty(self.db),

            ResolvedPathInExpr::Binding(_, binding) => self.env.lookup_binding_ty(binding),

            ResolvedPathInExpr::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInExpr::Invalid => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_if(&mut self, _expr: ExprId, expr_data: &Expr, expected: TyId) -> TyId {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        match else_ {
            Some(else_) => {
                self.check_expr_in_new_scope(*then, expected);
                self.check_expr_in_new_scope(*else_, expected)
            }

            None => {
                // If there is no else branch, the if expression itself typed as `()`
                let expected = self.fresh_ty();
                self.check_expr_in_new_scope(*then, expected);
                TyId::unit(self.db)
            }
        }
    }

    fn check_expr_in_new_scope(&mut self, expr: ExprId, expected: TyId) -> TyId {
        self.env.enter_scope(expr);
        let ty = self.check_expr(expr, expected);
        self.env.leave_scope();

        ty
    }
}
