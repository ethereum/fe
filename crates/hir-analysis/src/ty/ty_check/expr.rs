use hir::hir_def::{Expr, ExprId, FieldIndex, Partial, VariantKind};

use super::{path::ResolvedPathInExpr, RecordLike};
use crate::ty::{
    const_ty::ConstTyId,
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_check::{
        path::{
            resolve_path_in_expr, resolve_path_in_record_init, RecordInitChecker,
            ResolvedPathInRecordInit, TyInBody,
        },
        TyChecker,
    },
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
            Expr::Path(..) => self.check_path(expr, expr_data),
            Expr::RecordInit(..) => self.check_record_init(expr, expr_data),
            Expr::Field(..) => self.check_field(expr, expr_data),
            Expr::Tuple(..) => self.check_tuple(expr, expr_data, expected),
            Expr::Index(..) => todo!(),
            Expr::Array(..) => self.check_array(expr, expr_data, expected),
            Expr::ArrayRep(..) => self.check_array_rep(expr, expr_data, expected),
            Expr::If(..) => self.check_if(expr, expr_data),
            Expr::Match(..) => self.check_match(expr, expr_data),
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

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr) -> TyId {
        let Expr::Path(path) = expr_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_expr(self, *path, expr) {
            ResolvedPathInExpr::Ty(mut ty) => {
                if ty.is_func(self.db) {
                    ty.ty(self.db)
                } else {
                    // TODO: Refine this diagnostic.
                    let diag = BodyDiag::unit_variant_expected(
                        self.db,
                        expr.lazy_span(self.body()).into(),
                        ty,
                    )
                    .into();
                    FuncBodyDiagAccumulator::push(self.db, diag);

                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInExpr::Variant(mut variant) => {
                if matches!(variant.variant_kind(self.db), VariantKind::Unit) {
                    variant.ty(self.db)
                } else {
                    let diag = BodyDiag::unit_variant_expected(
                        self.db,
                        expr.lazy_span(self.body()).into(),
                        variant,
                    )
                    .into();
                    FuncBodyDiagAccumulator::push(self.db, diag);

                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInExpr::Binding(_, binding) => self.env.lookup_binding_ty(binding),

            ResolvedPathInExpr::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInExpr::Invalid => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_record_init(&mut self, expr: ExprId, expr_data: &Expr) -> TyId {
        let Expr::RecordInit(path, ..) = expr_data else {
            unreachable!()
        };
        let span = expr.lazy_span(self.body()).into_record_init_expr();

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_record_init(self, *path, expr) {
            ResolvedPathInRecordInit::Ty(mut ty_in_body) => {
                if ty_in_body.is_record(self.db) {
                    let ty = ty_in_body.ty(self.db);
                    self.check_record_init_fields(ty_in_body, expr);
                    ty
                } else {
                    let diag =
                        BodyDiag::record_expected(self.db, span.path().into(), Some(ty_in_body));
                    FuncBodyDiagAccumulator::push(self.db, diag.into());

                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInRecordInit::Variant(mut variant) => {
                let ty = variant.ty(self.db);
                self.check_record_init_fields(variant, expr);
                ty
            }

            ResolvedPathInRecordInit::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInRecordInit::Invalid => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_record_init_fields<T: RecordLike>(&mut self, mut record_like: T, expr: ExprId) {
        let hir_db = self.db.as_hir_db();

        let Partial::Present(Expr::RecordInit(_, fields)) = expr.data(hir_db, self.body()) else {
            unreachable!()
        };
        let span = expr.lazy_span(self.body()).into_record_init_expr();

        let mut rec_checker = RecordInitChecker::new(self, &mut record_like);

        for (i, field) in fields.iter().enumerate() {
            let label = field.label_eagerly(rec_checker.tc.db.as_hir_db(), rec_checker.tc.body());
            let field_span = span.fields().field(i).into();

            let expected = match rec_checker.feed_label(label, field_span) {
                Ok(ty) => ty,
                Err(diag) => {
                    FuncBodyDiagAccumulator::push(rec_checker.tc.db, diag);
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                }
            };

            rec_checker.tc.check_expr(field.expr, expected);
        }

        if let Err(diag) = rec_checker.finalize(span.fields().into(), false) {
            FuncBodyDiagAccumulator::push(self.db, diag);
        }
    }

    fn check_field(&mut self, expr: ExprId, expr_data: &Expr) -> TyId {
        let Expr::Field(indexed, index) = expr_data else {
            unreachable!()
        };
        let Partial::Present(field) = index else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let ty = self.fresh_ty();
        let ty = self
            .check_expr(*indexed, ty)
            .apply_subst(self.db, &mut self.table);
        let (ty_base, ty_args) = ty.decompose_ty_app(self.db);

        if ty_base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(indexed.lazy_span(self.body()).into());
            FuncBodyDiagAccumulator::push(self.db, diag.into());

            return TyId::invalid(self.db, InvalidCause::Other);
        }

        match field {
            FieldIndex::Ident(label) => {
                let mut ty_in_body = TyInBody::new(self.db, &mut self.table, ty);
                if let Some(ty) = ty_in_body.record_field_ty(self.db, *label) {
                    return ty;
                }
            }

            FieldIndex::Index(i) => {
                let arg_len = ty_args.len().into();
                if ty_base.is_tuple(self.db) && i.data(self.db.as_hir_db()) < &arg_len {
                    let i: usize = i.data(self.db.as_hir_db()).try_into().unwrap();
                    return ty_args[i];
                }
            }
        };

        let diag = BodyDiag::accessed_field_not_found(
            self.db,
            expr.lazy_span(self.body()).into(),
            ty,
            *field,
        );
        FuncBodyDiagAccumulator::push(self.db, diag.into());

        TyId::invalid(self.db, InvalidCause::Other)
    }

    fn check_tuple(&mut self, _expr: ExprId, expr_data: &Expr, expected: TyId) -> TyId {
        let Expr::Tuple(elems) = expr_data else {
            unreachable!()
        };

        let elem_tys = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) && args.len() == elems.len() => args.to_vec(),
            _ => self.fresh_tys_n(elems.len()),
        };

        for (elem, elem_ty) in elems.iter().zip(elem_tys.iter()) {
            self.check_expr(*elem, *elem_ty);
        }

        TyId::tuple_with_elems(self.db, &elem_tys)
    }

    fn check_array(&mut self, _expr: ExprId, expr_data: &Expr, expected: TyId) -> TyId {
        let Expr::Array(elems) = expr_data else {
            unreachable!()
        };

        let expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        for elem in elems {
            self.check_expr(*elem, expected_elem_ty);
        }

        TyId::array_with_elem(self.db, expected_elem_ty, elems.len())
    }

    fn check_array_rep(&mut self, _expr: ExprId, expr_data: &Expr, expected: TyId) -> TyId {
        let Expr::ArrayRep(elem, len) = expr_data else {
            unreachable!()
        };

        let expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        self.check_expr(*elem, expected_elem_ty);

        let array = TyId::app(self.db, TyId::array(self.db), expected_elem_ty);
        if let Some(len_body) = len.to_opt() {
            let len_ty = ConstTyId::from_body(self.db, len_body);
            let len_ty = TyId::const_ty(self.db, len_ty);
            let array_ty = TyId::app(self.db, array, len_ty);

            if let Some(diag) = array_ty.emit_diag(self.db, len_body.lazy_span().into()) {
                FuncBodyDiagAccumulator::push(self.db, diag.into());
            }

            array_ty
        } else {
            let len_ty = ConstTyId::invalid(self.db, InvalidCause::Other);
            let len_ty = TyId::const_ty(self.db, len_ty);
            TyId::app(self.db, array, len_ty)
        }
    }

    fn check_if(&mut self, _expr: ExprId, expr_data: &Expr) -> TyId {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        let if_ty = self.fresh_ty();
        match else_ {
            Some(else_) => {
                self.check_expr_in_new_scope(*then, if_ty);
                self.check_expr_in_new_scope(*else_, if_ty)
            }

            None => {
                // If there is no else branch, the if expression itself typed as `()`
                self.check_expr_in_new_scope(*then, if_ty);
                TyId::unit(self.db)
            }
        }
    }

    fn check_match(&mut self, _expr: ExprId, expr_data: &Expr) -> TyId {
        let Expr::Match(scrutinee, arms) = expr_data else {
            unreachable!()
        };

        let scrutinee_ty = self.fresh_ty();
        self.check_expr(*scrutinee, scrutinee_ty);

        let Partial::Present(arms) = arms else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let match_ty = self.fresh_ty();
        for arm in arms {
            self.check_pat(arm.pat, scrutinee_ty);

            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();

            self.check_expr(arm.body, match_ty);

            self.env.leave_scope();
        }

        match_ty
    }

    fn check_expr_in_new_scope(&mut self, expr: ExprId, expected: TyId) -> TyId {
        self.env.enter_scope(expr);
        let ty = self.check_expr(expr, expected);
        self.env.leave_scope();

        ty
    }
}
