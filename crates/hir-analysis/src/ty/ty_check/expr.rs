use either::Either;
use hir::hir_def::{
    ArithBinOp, BinOp, Expr, ExprId, FieldIndex, GenericArgListId, IdentId, Partial, PathId, UnOp,
    VariantKind,
};

use super::{
    env::{ExprProp, LocalBinding, TyCheckEnv},
    path::ResolvedPathInBody,
    RecordLike, Typeable,
};
use crate::{
    name_resolution::{
        diagnostics::NameResDiag,
        is_scope_visible_from,
        method_selection::{select_method_candidate, Candidate, MethodSelectionError},
        resolve_name_res, resolve_query, EarlyNameQueryId, NameDomain, NameResBucket, PathRes,
        QueryDirective,
    },
    ty::{
        canonical::Canonicalized,
        const_ty::ConstTyId,
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_check::{callable::Callable, path::RecordInitChecker, TyChecker},
        ty_def::{InvalidCause, TyId},
    },
    HirAnalysisDb, Spanned,
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_expr(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let typed = ExprProp::invalid(self.db);
            self.env.type_expr(expr, typed);
            return typed;
        };

        let mut actual = match expr_data {
            Expr::Lit(lit) => {
                let ty = self.lit_ty(lit);
                ExprProp::new(ty, true)
            }
            Expr::Block(..) => self.check_block(expr, expr_data, expected),
            Expr::Un(..) => self.check_unary(expr, expr_data),
            Expr::Bin(..) => self.check_binary(expr, expr_data),
            Expr::Call(..) => self.check_call(expr, expr_data),
            Expr::MethodCall(..) => self.check_method_call(expr, expr_data),
            Expr::Path(..) => self.check_path(expr, expr_data),
            Expr::RecordInit(..) => self.check_record_init(expr, expr_data),
            Expr::Field(..) => self.check_field(expr, expr_data),
            Expr::Tuple(..) => self.check_tuple(expr, expr_data, expected),
            Expr::Index(..) => self.check_index(expr, expr_data),
            Expr::Array(..) => self.check_array(expr, expr_data, expected),
            Expr::ArrayRep(..) => self.check_array_rep(expr, expr_data, expected),
            Expr::If(..) => self.check_if(expr, expr_data),
            Expr::Match(..) => self.check_match(expr, expr_data),
            Expr::Assign(..) => self.check_assign(expr, expr_data),
            Expr::AugAssign(..) => self.check_aug_assign(expr, expr_data),
        };

        let typeable = Typeable::Expr(expr, actual);
        let ty = self.unify_ty(typeable, actual.ty, expected);
        actual.swap_ty(ty);
        actual
    }

    fn check_block(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Block(stmts) = expr_data else {
            unreachable!()
        };

        if stmts.is_empty() {
            ExprProp::new(TyId::unit(self.db), true)
        } else {
            self.env.enter_scope(expr);
            for &stmt in stmts[..stmts.len() - 1].iter() {
                let ty = self.fresh_ty();
                self.check_stmt(stmt, ty);
            }

            let last_stmt = stmts[stmts.len() - 1];
            let res = self.check_stmt(last_stmt, expected);
            self.env.leave_scope();
            ExprProp::new(res, true)
        }
    }

    fn check_unary(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Un(lhs, op) = expr_data else {
            unreachable!()
        };
        let Partial::Present(op) = op else {
            return ExprProp::invalid(self.db);
        };

        let expr_ty = self.fresh_ty();
        let typed_expr = self.check_expr(*lhs, expr_ty);
        let expr_ty = typed_expr.ty;

        if expr_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        match op {
            UnOp::Plus | UnOp::Minus => {
                if expr_ty.is_integral(self.db) {
                    return typed_expr;
                }
            }

            UnOp::Not => {
                if expr_ty.is_bool(self.db) {
                    return typed_expr;
                }
            }

            UnOp::BitNot => {
                if expr_ty.is_integral(self.db) {
                    return typed_expr;
                }
            }
        }

        let base_ty = expr_ty.base_ty(self.db);
        if base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // TODO: We need to check if the type implements a trait corresponding to the
        // operator when these traits are defined in `std`.
        let diag = BodyDiag::ops_trait_not_implemented(
            self.db,
            expr.lazy_span(self.body()).into(),
            expr_ty,
            *op,
        );
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    fn check_binary(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Bin(lhs, rhs, op) = expr_data else {
            unreachable!()
        };
        let Partial::Present(op) = op else {
            return ExprProp::invalid(self.db);
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        if lhs_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        match op {
            BinOp::Arith(arith_op) => {
                use hir::hir_def::ArithBinOp::*;

                let typed_rhs = self.check_expr(*rhs, lhs_ty);
                let rhs_ty = typed_rhs.ty;
                if rhs_ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                match arith_op {
                    Add | Sub | Mul | Div | Rem | Pow | LShift | RShift => {
                        if lhs_ty.is_integral(self.db) {
                            return typed_rhs;
                        }
                    }

                    BitAnd | BitOr | BitXor => {
                        if lhs_ty.is_integral(self.db) | lhs_ty.is_bool(self.db) {
                            return typed_rhs;
                        }
                    }
                }
            }

            BinOp::Comp(comp_op) => {
                use hir::hir_def::CompBinOp::*;

                let typed_rhs = self.check_expr(*rhs, lhs_ty);
                let rhs_ty = typed_rhs.ty;
                if rhs_ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                match comp_op {
                    Eq | NotEq => {
                        if lhs_ty.is_integral(self.db) | lhs_ty.is_bool(self.db) {
                            let ty = TyId::bool(self.db);
                            return ExprProp::new(ty, true);
                        }
                    }

                    Lt | LtEq | Gt | GtEq => {
                        if lhs_ty.is_integral(self.db) {
                            let ty = TyId::bool(self.db);
                            return ExprProp::new(ty, true);
                        }
                    }
                }
            }

            BinOp::Logical(logical_op) => {
                use hir::hir_def::LogicalBinOp::*;

                let typed_rhs = self.check_expr(*rhs, lhs_ty);
                let rhs_ty = typed_rhs.ty;
                if rhs_ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                match logical_op {
                    And | Or => {
                        if lhs_ty.is_bool(self.db) & rhs_ty.is_bool(self.db) {
                            let ty = TyId::bool(self.db);
                            return ExprProp::new(ty, true);
                        }
                    }
                }
            }
        }

        let lhs_base_ty = lhs_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // TODO: We need to check if the type implements a trait corresponding to the
        // operator when these traits are defined in `std`.
        let diag = BodyDiag::ops_trait_not_implemented(
            self.db,
            expr.lazy_span(self.body()).into(),
            lhs_ty,
            *op,
        );
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    fn check_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Call(callee, args) = expr_data else {
            unreachable!()
        };
        let callee_ty = self.fresh_ty();
        let callee_ty = self.check_expr(*callee, callee_ty).ty;

        if callee_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let mut callable =
            match Callable::new(self.db, callee_ty, callee.lazy_span(self.body()).into()) {
                Ok(callable) => callable,
                Err(diag) => {
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
            };

        let call_span = expr.lazy_span(self.body()).into_call_expr();

        if let Partial::Present(Expr::Path(Partial::Present(path))) =
            callee.data(self.db, self.body())
        {
            let idx = path.segment_index(self.db);

            if !callable.unify_generic_args(
                self,
                path.generic_args(self.db),
                expr.lazy_span(self.body())
                    .into_path_expr()
                    .path()
                    .segment(idx)
                    .generic_args(),
            ) {
                return ExprProp::invalid(self.db);
            }
        };

        callable.check_args(self, args, call_span.args_moved(), None);

        let ret_ty = callable.ret_ty(self.db);
        self.env.register_callable(expr, callable);
        ExprProp::new(ret_ty, true)
    }

    fn check_method_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::MethodCall(receiver, method_name, generic_args, args) = expr_data else {
            unreachable!()
        };
        let call_span = expr.lazy_span(self.body()).into_method_call_expr();
        let Some(method_name) = method_name.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let receiver_prop = self.fresh_ty();
        let receiver_prop = self.check_expr(*receiver, receiver_prop);
        if receiver_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let assumptions = self.env.assumptions();

        let canonical_r_ty = Canonicalized::new(self.db, receiver_prop.ty);
        let candidate = match select_method_candidate(
            self.db,
            canonical_r_ty.value,
            method_name,
            self.env.scope(),
            assumptions,
        ) {
            Ok(candidate) => candidate,
            Err(diag) => {
                let diag = xxx_err_thing(
                    self.db,
                    diag,
                    Spanned::new(
                        canonical_r_ty.value.value,
                        receiver.lazy_span(self.body()).into(),
                    ),
                    Spanned::new(method_name, call_span.method_name().into()),
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        let func_ty = match candidate {
            Candidate::InherentMethod(func_def) => {
                let func_ty = TyId::func(self.db, func_def);
                self.table.instantiate_to_term(func_ty)
            }

            Candidate::TraitMethod(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let trait_method = cand.method;
                trait_method.instantiate_with_inst(&mut self.table, receiver_prop.ty, inst)
            }

            Candidate::NeedsConfirmation(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                self.env
                    .register_confirmation(inst, call_span.clone().into());
                let trait_method = cand.method;
                trait_method.instantiate_with_inst(&mut self.table, receiver_prop.ty, inst)
            }
        };

        let mut callable =
            match Callable::new(self.db, func_ty, receiver.lazy_span(self.body()).into()) {
                Ok(callable) => callable,
                Err(diag) => {
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
            };

        if !callable.unify_generic_args(self, *generic_args, call_span.generic_args()) {
            return ExprProp::invalid(self.db);
        }

        if !callable.func_def.is_method(self.db) {
            let diag = BodyDiag::NotAMethod {
                span: call_span,
                receiver_ty: receiver_prop.ty,
                func_name: method_name,
                func_ty,
            };
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        callable.check_args(
            self,
            args,
            call_span.args_moved(),
            Some((*receiver, receiver_prop)),
        );
        let ret_ty = callable.ret_ty(self.db);
        self.env.register_callable(expr, callable);
        ExprProp::new(ret_ty, true)
    }

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Path(path) = expr_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let span = expr.lazy_span(self.body()).into_path_expr();

        let res = if path.is_bare_ident(self.db) {
            resolve_ident_expr(self.db, &self.env, *path)
        } else {
            self.resolve_path(*path, true)
                .map_or_else(|_| ResolvedPathInBody::Invalid, ResolvedPathInBody::Reso)
        };

        match res {
            ResolvedPathInBody::Binding(binding) => {
                let ty = self.env.lookup_binding_ty(binding);
                let is_mut = binding.is_mut();
                ExprProp::new_binding_ref(ty, is_mut, binding)
            }
            ResolvedPathInBody::NewBinding(ident) => {
                let diag = BodyDiag::UndefinedVariable(span.into(), ident);
                self.push_diag(diag);

                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Diag(diag) => {
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Invalid => ExprProp::invalid(self.db),

            ResolvedPathInBody::Reso(reso) => match reso {
                PathRes::Ty(ty) => {
                    if let Some(const_ty_ty) = ty.const_ty_ty(self.db) {
                        ExprProp::new(self.table.instantiate_to_term(const_ty_ty), true)
                    } else {
                        let diag = if ty.is_struct(self.db) {
                            BodyDiag::unit_variant_expected(self.db, span.into(), ty)
                        } else {
                            BodyDiag::NotValue {
                                primary: span.into(),
                                given: Either::Right(ty),
                            }
                        };
                        self.push_diag(diag);

                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::Func(ty) => ExprProp::new(self.table.instantiate_to_term(ty), true),
                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.trait_(self.db).into()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::EnumVariant(variant) => {
                    let ty = match variant.variant_kind(self.db) {
                        VariantKind::Unit => variant.ty,
                        VariantKind::Tuple(_) => {
                            let ty = variant.constructor_func_ty(self.db).unwrap();
                            self.table.instantiate_to_term(ty)
                        }
                        VariantKind::Record(_) => {
                            let diag = BodyDiag::unit_variant_expected(
                                self.db,
                                expr.lazy_span(self.body()).into(),
                                variant,
                            );
                            self.push_diag(diag);

                            TyId::invalid(self.db, InvalidCause::Other)
                        }
                    };

                    ExprProp::new(self.table.instantiate_to_term(ty), true)
                }
                PathRes::Const(ty) => ExprProp::new(ty, true),
                PathRes::Method(receiver_ty, candidate) => {
                    let canonical_r_ty = Canonicalized::new(self.db, receiver_ty);
                    let method_ty = match candidate {
                        Candidate::InherentMethod(func_def) => {
                            // xxx move this stuff to path_resolver
                            let mut method_ty = TyId::func(self.db, func_def);
                            for &arg in receiver_ty.generic_args(self.db) {
                                // If the method is defined in "specialized" impl block
                                // of a generic type (eg `impl Option<i32>`), then
                                // calling `TyId::app(db, method_ty, ..)` will result in
                                // `TyId::invalid`.
                                if method_ty.applicable_ty(self.db).is_some() {
                                    method_ty = TyId::app(self.db, method_ty, arg);
                                } else {
                                    break;
                                }
                            }
                            method_ty
                        }
                        Candidate::TraitMethod(cand) | Candidate::NeedsConfirmation(cand) => {
                            let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                            if matches!(candidate, Candidate::NeedsConfirmation(_)) {
                                self.env.register_confirmation(inst, span.clone().into());
                            }
                            cand.method
                                .instantiate_with_inst(&mut self.table, receiver_ty, inst)
                        }
                    };
                    ExprProp::new(self.table.instantiate_to_term(method_ty), true)
                }
                PathRes::Mod(_) | PathRes::FuncParam(..) => todo!(),
            },
        }
    }

    fn check_record_init(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::RecordInit(path, ..) = expr_data else {
            unreachable!()
        };
        let span = expr.lazy_span(self.body()).into_record_init_expr();

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let Ok(reso) = self.resolve_path(*path, true) else {
            return ExprProp::invalid(self.db);
        };

        match reso {
            PathRes::Ty(ty) if ty.is_record(self.db) => {
                self.check_record_init_fields(&ty, expr);
                ExprProp::new(ty, true)
            }

            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => {
                let diag = BodyDiag::record_expected(self.db, span.path().into(), Some(ty));
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::Method(..) | PathRes::FuncParam(..) => {
                let diag = BodyDiag::record_expected::<TyId>(self.db, span.path().into(), None);
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }

            PathRes::EnumVariant(variant) => {
                if variant.is_record(self.db) {
                    self.check_record_init_fields(&variant, expr);
                    ExprProp::new(variant.ty, true)
                } else {
                    let diag =
                        BodyDiag::record_expected::<TyId<'db>>(self.db, span.path().into(), None);
                    self.push_diag(diag);

                    ExprProp::invalid(self.db)
                }
            }
            PathRes::Mod(scope) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(scope.item()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::Trait(trait_) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(trait_.trait_(self.db).into()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
        }
    }

    fn check_record_init_fields<T: RecordLike<'db>>(&mut self, record_like: &T, expr: ExprId) {
        let hir_db = self.db;

        let Partial::Present(Expr::RecordInit(_, fields)) = expr.data(hir_db, self.body()) else {
            unreachable!()
        };
        let span = expr.lazy_span(self.body()).into_record_init_expr();

        let mut rec_checker = RecordInitChecker::new(self, record_like);

        for (i, field) in fields.iter().enumerate() {
            let label = field.label_eagerly(rec_checker.tc.db, rec_checker.tc.body());
            let field_span = span.fields().field(i).into();

            let expected = match rec_checker.feed_label(label, field_span) {
                Ok(ty) => ty,
                Err(diag) => {
                    rec_checker.tc.push_diag(diag);
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                }
            };

            rec_checker.tc.check_expr(field.expr, expected);
        }

        if let Err(diag) = rec_checker.finalize(span.fields().into(), false) {
            self.push_diag(diag);
        }
    }

    fn check_field(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Field(lhs, index) = expr_data else {
            unreachable!()
        };
        let Partial::Present(field) = index else {
            return ExprProp::invalid(self.db);
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        let (ty_base, ty_args) = lhs_ty.decompose_ty_app(self.db);

        if ty_base.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if ty_base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        match field {
            FieldIndex::Ident(label) => {
                if let Some(field_ty) = lhs_ty.record_field_ty(self.db, *label) {
                    if let Some(scope) = lhs_ty.record_field_scope(self.db, *label) {
                        if !is_scope_visible_from(self.db, scope, self.env.scope()) {
                            // Check the visibility of the field.
                            let diag = NameResDiag::Invisible(
                                expr.lazy_span(self.body())
                                    .into_field_expr()
                                    .accessor()
                                    .into(),
                                *label,
                                scope.name_span(self.db),
                            );

                            self.push_diag(diag);
                            return ExprProp::invalid(self.db);
                        }
                    }
                    return ExprProp::new(field_ty, typed_lhs.is_mut);
                }
            }

            FieldIndex::Index(i) => {
                let arg_len = ty_args.len().into();
                if ty_base.is_tuple(self.db) && i.data(self.db) < &arg_len {
                    let i: usize = i.data(self.db).try_into().unwrap();
                    let ty = ty_args[i];
                    return ExprProp::new(ty, typed_lhs.is_mut);
                }
            }
        };

        let diag = BodyDiag::AccessedFieldNotFound {
            primary: expr.lazy_span(self.body()).into(),
            given_ty: lhs_ty,
            index: *field,
        };
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    fn check_tuple(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
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

        let ty = TyId::tuple_with_elems(self.db, &elem_tys);
        ExprProp::new(ty, true)
    }

    fn check_index(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Index(lhs, index) = expr_data else {
            unreachable!()
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        let (lhs_base, args) = lhs_ty.decompose_ty_app(self.db);

        if lhs_base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if lhs_base.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if lhs_base.is_array(self.db) {
            let elem_ty = args[0];
            let index_ty = args[1].const_ty_ty(self.db).unwrap();
            self.check_expr(*index, index_ty);
            return ExprProp::new(elem_ty, typed_lhs.is_mut);
        }

        // TODO: We need to check if the type implements the `Index` trait when `Index`
        // is defined in `std`.
        let diag = BodyDiag::ops_trait_not_implemented(
            self.db,
            expr.lazy_span(self.body()).into(),
            lhs_ty,
            IndexingOp {},
        );
        self.push_diag(diag);
        ExprProp::invalid(self.db)
    }

    fn check_array(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Array(elems) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        for elem in elems {
            expected_elem_ty = self.check_expr(*elem, expected_elem_ty).ty;
        }

        let ty = TyId::array_with_len(self.db, expected_elem_ty, elems.len());
        ExprProp::new(ty, true)
    }

    fn check_array_rep(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::ArrayRep(elem, len) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        expected_elem_ty = self.check_expr(*elem, expected_elem_ty).ty;

        let array = TyId::array(self.db, expected_elem_ty);
        let ty = if let Some(len_body) = len.to_opt() {
            let len_ty = ConstTyId::from_body(self.db, len_body);
            let len_ty = TyId::const_ty(self.db, len_ty);
            let array_ty = TyId::app(self.db, array, len_ty);

            if let Some(diag) = array_ty.emit_diag(self.db, len_body.lazy_span().into()) {
                self.push_diag(diag);
            }

            array_ty
        } else {
            let len_ty = ConstTyId::invalid(self.db, InvalidCause::Other);
            let len_ty = TyId::const_ty(self.db, len_ty);
            TyId::app(self.db, array, len_ty)
        };

        ExprProp::new(ty, true)
    }

    fn check_if(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        let if_ty = self.fresh_ty();
        let ty = match else_ {
            Some(else_) => {
                self.check_expr_in_new_scope(*then, if_ty);
                self.check_expr_in_new_scope(*else_, if_ty).ty
            }

            None => {
                // If there is no else branch, the if expression itself typed as `()`
                self.check_expr_in_new_scope(*then, if_ty);
                TyId::unit(self.db)
            }
        };

        ExprProp::new(ty, true)
    }

    fn check_match(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Match(scrutinee, arms) = expr_data else {
            unreachable!()
        };

        let scrutinee_ty = self.fresh_ty();
        let scrutinee_ty = self.check_expr(*scrutinee, scrutinee_ty).ty;

        let Partial::Present(arms) = arms else {
            return ExprProp::invalid(self.db);
        };

        let mut match_ty = self.fresh_ty();
        for arm in arms {
            self.check_pat(arm.pat, scrutinee_ty);

            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();

            match_ty = self.check_expr(arm.body, match_ty).ty;

            self.env.leave_scope();
        }

        ExprProp::new(match_ty, true)
    }

    fn check_assign(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Assign(lhs, rhs) = expr_data else {
            unreachable!()
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        self.check_expr(*rhs, lhs_ty);

        let result_ty = TyId::unit(self.db);

        self.check_assign_lhs(*lhs, &typed_lhs);

        ExprProp::new(result_ty, true)
    }

    fn check_aug_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        use ArithBinOp::*;

        let Expr::AugAssign(lhs, rhs, op) = expr_data else {
            unreachable!()
        };

        let unit_ty = TyId::unit(self.db);

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        let lhs_ty = typed_lhs.ty;
        if lhs_ty.has_invalid(self.db) {
            return ExprProp::new(unit_ty, true);
        }

        match op {
            Add | Sub | Mul | Div | Rem | Pow | LShift | RShift => {
                self.check_expr(*rhs, lhs_ty);
                if lhs_ty.is_integral(self.db) {
                    self.check_assign_lhs(*lhs, &typed_lhs);
                    return ExprProp::new(unit_ty, true);
                }
            }

            BitAnd | BitOr | BitXor => {
                self.check_expr(*rhs, lhs_ty);
                if lhs_ty.is_integral(self.db) | lhs_ty.is_bool(self.db) {
                    self.check_assign_lhs(*lhs, &typed_lhs);
                    return ExprProp::new(unit_ty, true);
                }
            }
        }

        let lhs_base_ty = lhs_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // TODO: We need to check if the type implements a trait corresponding to the
        // operator when these traits are defined in `std`.
        let diag = BodyDiag::ops_trait_not_implemented(
            self.db,
            expr.lazy_span(self.body()).into(),
            lhs_ty,
            AugAssignOp(*op),
        );
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    fn check_assign_lhs(&mut self, lhs: ExprId, typed_lhs: &ExprProp<'db>) {
        if !self.is_assignable_expr(lhs) {
            let diag = BodyDiag::NonAssignableExpr(lhs.lazy_span(self.body()).into());
            self.push_diag(diag);

            return;
        }

        if !typed_lhs.is_mut {
            let binding = self.find_base_binding(lhs);
            let diag = match binding {
                Some(binding) => {
                    let (ident, def_span) = (
                        self.env.binding_name(binding),
                        self.env.binding_def_span(binding),
                    );

                    BodyDiag::ImmutableAssignment {
                        primary: lhs.lazy_span(self.body()).into(),
                        binding: Some((ident, def_span)),
                    }
                }

                None => BodyDiag::ImmutableAssignment {
                    primary: lhs.lazy_span(self.body()).into(),
                    binding: None,
                },
            };

            self.push_diag(diag);
        }
    }

    fn check_expr_in_new_scope(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        self.env.enter_scope(expr);
        let ty = self.check_expr(expr, expected);
        self.env.leave_scope();

        ty
    }

    /// Returns the base binding for a given expression if it exists.
    ///
    /// This function traverses the expression tree to find the base binding,
    /// which is the original variable or binding that the expression refers to.
    ///
    /// # Parameters
    ///
    /// - `expr`: The expression ID for which to find the base binding.
    ///
    /// # Returns
    ///
    /// An `Option` containing the `LocalBinding` if a base binding is found,
    /// or `None` if there is no base binding.
    fn find_base_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Field(lhs, ..) | Expr::Index(lhs, ..) => self.find_base_binding(*lhs),
            Expr::Path(..) => self.env.typed_expr(expr)?.binding(),
            _ => None,
        }
    }

    /// Returns `true`` if the expression can be used as an left hand side of an
    /// assignment.
    /// This method doesn't take mutability into account.
    fn is_assignable_expr(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        matches!(
            expr_data,
            Expr::Path(..) | Expr::Field(..) | Expr::Index(..)
        )
    }
}

fn xxx_err_thing<'db>(
    db: &'db dyn HirAnalysisDb,
    err: MethodSelectionError<'db>,
    receiver: Spanned<'db, TyId<'db>>,
    method: Spanned<'db, IdentId<'db>>,
) -> FuncBodyDiag<'db> {
    match err {
        MethodSelectionError::ReceiverTypeMustBeKnown => {
            BodyDiag::TypeMustBeKnown(receiver.span).into()
        }
        MethodSelectionError::AmbiguousInherentMethod(candidates) => {
            BodyDiag::AmbiguousInherentMethodCall {
                primary: method.span,
                method_name: method.data,
                candidates,
            }
            .into()
        }

        MethodSelectionError::AmbiguousTraitMethod(traits) => {
            let traits = traits.into_iter().map(|def| def.trait_(db)).collect();

            BodyDiag::AmbiguousTrait {
                primary: method.span,
                method_name: method.data,
                traits,
            }
            .into()
        }

        MethodSelectionError::NotFound => {
            let base_ty = receiver.data.base_ty(db);
            BodyDiag::MethodNotFound {
                primary: method.span,
                method_name: method.data,
                receiver: Either::Left(base_ty),
            }
            .into()
        }

        MethodSelectionError::InvisibleInherentMethod(func) => {
            NameResDiag::Invisible(method.span, method.data, func.name_span(db).into()).into()
        }

        MethodSelectionError::InvisibleTraitMethod(traits) => BodyDiag::InvisibleAmbiguousTrait {
            primary: method.span,
            traits,
        }
        .into(),
    }
}

fn resolve_ident_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    env: &TyCheckEnv<'db>,
    path: PathId<'db>,
) -> ResolvedPathInBody<'db> {
    let ident = *path.ident(db).unwrap();

    let resolve_bucket = |bucket: &NameResBucket<'db>, scope| {
        let Ok(res) = bucket.pick_any(&[NameDomain::VALUE, NameDomain::TYPE]) else {
            return ResolvedPathInBody::Invalid;
        };
        let Ok(reso) = resolve_name_res(db, res, None, path, scope) else {
            return ResolvedPathInBody::Invalid;
        };
        ResolvedPathInBody::Reso(reso)
    };

    let mut current_idx = env.current_block_idx();

    loop {
        let block = env.get_block(current_idx);
        if let Some(binding) = block.lookup_var(ident) {
            return ResolvedPathInBody::Binding(binding);
        }

        let scope = block.scope;
        let directive = QueryDirective::new().disallow_lex();
        let query = EarlyNameQueryId::new(db, ident, scope, directive);
        let bucket = resolve_query(db, query);

        let resolved = resolve_bucket(bucket, scope);
        match resolved {
            ResolvedPathInBody::Invalid => {
                if current_idx == 0 {
                    break;
                } else {
                    current_idx -= 1;
                }
            }
            _ => return resolved,
        }
    }

    let query = EarlyNameQueryId::new(db, ident, env.body().scope(), QueryDirective::default());
    let bucket = resolve_query(db, query);
    match resolve_bucket(bucket, env.scope()) {
        ResolvedPathInBody::Invalid => ResolvedPathInBody::NewBinding(ident),
        r => r,
    }
}

/// This traits are intended to be implemented by the operators that can work as
/// a syntax sugar for a trait method. For example, binary `+` operator
/// implements this trait to be able to work as a syntax sugar for
/// `std::ops::Add` trait method.
///
/// TODO: We need to refine this trait definition to connect std library traits
/// smoothly.
pub(crate) trait TraitOps {
    fn trait_path<'db>(&self, db: &'db dyn HirAnalysisDb) -> PathId<'db> {
        let hir_db = db;
        let path = std_ops_path(db);
        path.push(
            hir_db,
            Partial::Present(self.trait_name(db)),
            GenericArgListId::none(hir_db),
        )
    }

    fn trait_name<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.triple(db)[0]
    }

    fn op_symbol<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        self.triple(db)[2]
    }

    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3];
}

impl TraitOps for UnOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        let triple = match self {
            UnOp::Plus => ["UnaryPlus", "add", "+"],
            UnOp::Minus => ["Neg", "neg", "-"],
            UnOp::Not => ["Not", "not", "!"],
            UnOp::BitNot => ["BitNot", "bit_not", "~"],
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

impl TraitOps for BinOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        let triple = match self {
            BinOp::Arith(arith_op) => {
                use ArithBinOp::*;

                match arith_op {
                    Add => ["Add", "add", "+"],
                    Sub => ["Sub", "sub", "-"],
                    Mul => ["Mul", "mul", "*"],
                    Div => ["Div", "div", "/"],
                    Rem => ["Rem", "rem", "%"],
                    Pow => ["Pow", "pow", "**"],
                    LShift => ["Shl", "shl", "<<"],
                    RShift => ["Shr", "shr", ">>"],
                    BitAnd => ["BitAnd", "bitand", "&"],
                    BitOr => ["BitOr", "bitor", "|"],
                    BitXor => ["BitXor", "bitxor", "^"],
                }
            }

            BinOp::Comp(comp_op) => {
                use hir::hir_def::CompBinOp::*;

                // Comp
                match comp_op {
                    Eq => ["Eq", "eq", "=="],
                    NotEq => ["Eq", "ne", "!="],
                    Lt => ["Ord", "lt", "<"],
                    LtEq => ["Ord", "le", "<="],
                    Gt => ["Ord", "gt", ">"],
                    GtEq => ["Ord", "ge", ">="],
                }
            }

            BinOp::Logical(logical_op) => {
                use hir::hir_def::LogicalBinOp::*;

                match logical_op {
                    And => ["And", "and", "&&"],
                    Or => ["Or", "or", "||"],
                }
            }
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

struct IndexingOp {}

impl TraitOps for IndexingOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        let name = "Index";
        let method_name = "index";
        let symbol = "[]";

        [
            IdentId::new(db, name.to_string()),
            IdentId::new(db, method_name.to_string()),
            IdentId::new(db, symbol.to_string()),
        ]
    }
}

struct AugAssignOp(ArithBinOp);

impl TraitOps for AugAssignOp {
    fn triple<'db>(&self, db: &'db dyn HirAnalysisDb) -> [IdentId<'db>; 3] {
        use ArithBinOp::*;
        let triple = match self.0 {
            Add => ["AddAssign", "add_assign", "+="],
            Sub => ["SubAssign", "sub_assign", "-="],
            Mul => ["MulAssign", "mul_assign", "*="],
            Div => ["DivAssign", "div_assign", "/="],
            Rem => ["RemAssign", "rem_assign", "%="],
            Pow => ["PowAssign", "pow_assign", "**="],
            LShift => ["ShlAssign", "shl_assign", "<<="],
            RShift => ["ShrAssign", "shr_assign", ">>="],
            BitAnd => ["BitAndAssign", "bitand_assign", "&="],
            BitOr => ["BitOrAssign", "bitor_assign", "|="],
            BitXor => ["BitXorAssign", "bitxor_assign", "^="],
        };

        triple.map(|s| IdentId::new(db, s.to_string()))
    }
}

fn std_ops_path(db: &dyn HirAnalysisDb) -> PathId {
    let std_ = IdentId::new(db, "std".to_string());
    let ops_ = IdentId::new(db, "ops".to_string());
    PathId::from_ident(db, std_).push_ident(db, ops_)
}
