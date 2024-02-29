use hir::hir_def::{Partial, Stmt, StmtId};

use super::TyChecker;
use crate::ty::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{InvalidCause, TyId},
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_stmt(&mut self, stmt: StmtId, expected: TyId) -> TyId {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match stmt_data {
            Stmt::Let(..) => self.check_let(stmt, stmt_data),

            Stmt::For(..) => todo!(),

            Stmt::While(..) => self.check_while(stmt, stmt_data),

            Stmt::Continue => self.check_continue(stmt, stmt_data),
            Stmt::Break => self.check_break(stmt, stmt_data),

            Stmt::Return(..) => self.check_return(stmt, stmt_data),

            Stmt::Expr(expr) => self.check_expr(*expr, expected).ty(),
        }
    }

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::Let(pat, ascription, expr) = stmt_data else {
            unreachable!()
        };

        let span = stmt.lazy_span(self.env.body()).into_let_stmt();

        let ascription = match ascription {
            Some(ty) => self.lower_ty(*ty, span.ty_moved().into(), true),
            None => self.fresh_ty(),
        };

        if let Some(expr) = expr {
            self.check_expr(*expr, ascription);
        }

        self.check_pat(*pat, ascription);
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }

    fn check_while(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::While(cond, body) = stmt_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        self.env.enter_loop(stmt);
        self.check_expr(*body, TyId::unit(self.db));
        self.env.leave_loop();

        TyId::unit(self.db)
    }

    fn check_continue(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        assert!(matches!(stmt_data, Stmt::Continue));

        if self.env.current_loop().is_none() {
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: false,
            };
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }

    fn check_break(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        assert!(matches!(stmt_data, Stmt::Break));

        if self.env.current_loop().is_none() {
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: true,
            };
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }

    fn check_return(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::Return(expr) = stmt_data else {
            unreachable!()
        };

        let returned_ty = if let Some(expr) = expr {
            let returned_ty = self.fresh_ty();
            self.check_expr(*expr, returned_ty);
            returned_ty.apply_subst(self.db, &mut self.table)
        } else {
            TyId::unit(self.db)
        };

        if self.table.unify(returned_ty, self.expected).is_err() {
            let func = self.env.func();
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::returned_type_mismatch(
                self.db,
                span.into(),
                returned_ty,
                self.expected,
                func,
            );

            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }
}
