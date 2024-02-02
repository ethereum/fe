use hir::hir_def::{Partial, Stmt, StmtId};

use super::TyChecker;
use crate::ty::ty_def::{InvalidCause, TyId};

impl<'db> TyChecker<'db> {
    pub(super) fn check_stmt(&mut self, stmt: StmtId, expected: TyId) -> TyId {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match stmt_data {
            Stmt::Let(..) => self.check_let(stmt, stmt_data),

            Stmt::For(..) => todo!(),

            Stmt::While(..) => todo!(),

            Stmt::Continue => todo!(),

            Stmt::Break => todo!(),

            Stmt::Return(..) => todo!(),

            Stmt::Expr(expr) => self.check_expr_ty(*expr, expected),
        }
    }

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::Let(pat, ascription, expr) = stmt_data else {
            unreachable!()
        };

        let span = stmt.lazy_span(self.env.body()).into_let_stmt();

        let ascription = match ascription {
            Some(ty) => self.lower_ty(*ty, span.ty_moved().into()),
            None => self.fresh_ty(),
        };

        self.check_pat(*pat, ascription);
        if let Some(expr) = expr {
            self.check_expr_ty(*expr, ascription);
        }

        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }
}
