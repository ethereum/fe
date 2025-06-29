use hir::{
    hir_def::{CallArg as HirCallArg, ExprId, GenericArgListId, IdentId},
    span::{
        expr::{LazyCallArgListSpan, LazyCallArgSpan},
        params::LazyGenericArgListSpan,
        DynLazySpan,
    },
};
use if_chain::if_chain;
use salsa::Update;

use super::{ExprProp, TyChecker};
use crate::{
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag},
        fold::{TyFoldable, TyFolder},
        func_def::FuncDef,
        ty_def::{TyBase, TyData, TyId},
        ty_lower::lower_generic_arg_list,
        visitor::{TyVisitable, TyVisitor},
    },
    HirAnalysisDb,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct Callable<'db> {
    pub func_def: FuncDef<'db>,
    generic_args: Vec<TyId<'db>>,
}

impl<'db> TyVisitable<'db> for Callable<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.generic_args.visit_with(visitor)
    }
}

impl<'db> TyFoldable<'db> for Callable<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            func_def: self.func_def,
            generic_args: self.generic_args.fold_with(folder),
        }
    }
}

impl<'db> Callable<'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> Result<Self, FuncBodyDiag<'db>> {
        let (base, args) = ty.decompose_ty_app(db);

        if base.is_ty_var(db) {
            return Err(BodyDiag::TypeMustBeKnown(span).into());
        }

        let TyData::TyBase(TyBase::Func(func_def)) = base.data(db) else {
            return Err(BodyDiag::NotCallable(span, ty).into());
        };

        let params = ty.generic_args(db);
        assert_eq!(params.len(), args.len());

        Ok(Self {
            func_def: *func_def,
            generic_args: args.to_vec(),
        })
    }

    pub fn generic_args(&self) -> &[TyId<'db>] {
        &self.generic_args
    }

    pub fn ret_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.func_def.ret_ty(db).instantiate(db, &self.generic_args)
    }

    pub fn ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ty = TyId::func(db, self.func_def);
        TyId::foldl(db, ty, &self.generic_args)
    }

    pub(super) fn unify_generic_args(
        &mut self,
        tc: &mut TyChecker<'db>,
        args: GenericArgListId<'db>,
        span: LazyGenericArgListSpan<'db>,
    ) -> bool {
        let db = tc.db;
        if !args.is_given(db) {
            return true;
        }

        let given_args = lower_generic_arg_list(db, args, tc.env.scope());
        let offset = self.func_def.offset_to_explicit_params_position(db);
        let current_args = &mut self.generic_args[offset..];

        if current_args.len() != given_args.len() {
            let diag = BodyDiag::CallGenericArgNumMismatch {
                primary: span.into(),
                def_span: self.func_def.name_span(db),
                given: given_args.len(),
                expected: current_args.len(),
            };
            tc.push_diag(diag);

            return false;
        }

        for (i, (&given, arg)) in given_args.iter().zip(current_args.iter_mut()).enumerate() {
            *arg = tc.equate_ty(given, *arg, span.clone().arg(i).into());
        }

        true
    }

    pub(super) fn check_args(
        &self,
        tc: &mut TyChecker<'db>,
        call_args: &[HirCallArg<'db>],
        span: LazyCallArgListSpan<'db>,
        receiver: Option<(ExprId, ExprProp<'db>)>,
    ) {
        let db = tc.db;

        let expected_arity = self.func_def.arg_tys(db).len();
        let given_arity = if receiver.is_some() {
            call_args.len() + 1
        } else {
            call_args.len()
        };
        if given_arity != expected_arity {
            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                def_span: self.func_def.name_span(db),
                given: given_arity,
                expected: expected_arity,
            };
            tc.push_diag(diag);
            return;
        }

        let mut args = if let Some((receiver_expr, receiver_prop)) = receiver {
            let mut args = Vec::with_capacity(call_args.len() + 1);
            let arg = CallArg::new(
                IdentId::make_self(db).into(),
                receiver_prop,
                None,
                receiver_expr.span(tc.body()).into(),
            );
            args.push(arg);
            args
        } else {
            Vec::with_capacity(call_args.len())
        };

        for (i, hir_arg) in call_args.iter().enumerate() {
            let arg = CallArg::from_hir_arg(tc, hir_arg, span.clone().arg(i));
            args.push(arg);
        }

        for (i, (given, expected)) in args
            .into_iter()
            .zip(self.func_def.arg_tys(db).iter())
            .enumerate()
        {
            if_chain! {
                // xxx check this
                if let Some(expected_label) = self.func_def.param_label(db, i);
                if !expected_label.is_self(db);
                if Some(expected_label) != given.label;
                then {
                    let diag = BodyDiag::CallArgLabelMismatch {
                        primary: given.label_span.unwrap_or(given.expr_span.clone()),
                        def_span: self.func_def.name_span(db),
                        given: given.label,
                        expected: expected_label,
                    };
                    tc.push_diag(diag);
                }
            }

            let expected = expected.instantiate(db, &self.generic_args);
            tc.equate_ty(given.expr_prop.ty, expected, given.expr_span);
        }
    }
}

/// The lowered representation of [`HirCallArg`]
struct CallArg<'db> {
    label: Option<IdentId<'db>>,
    expr_prop: ExprProp<'db>,
    label_span: Option<DynLazySpan<'db>>,
    expr_span: DynLazySpan<'db>,
}

impl<'db> CallArg<'db> {
    fn from_hir_arg(
        tc: &mut TyChecker<'db>,
        arg: &HirCallArg<'db>,
        span: LazyCallArgSpan<'db>,
    ) -> Self {
        let ty = tc.fresh_ty();
        let expr_prop = tc.check_expr(arg.expr, ty);
        let label = arg.label_eagerly(tc.db, tc.body());
        let label_span = arg.label.is_some().then(|| span.clone().label().into());
        let expr_span = span.expr().into();

        Self::new(label, expr_prop, label_span, expr_span)
    }

    fn new(
        label: Option<IdentId<'db>>,
        expr_prop: ExprProp<'db>,
        label_span: Option<DynLazySpan<'db>>,
        expr_span: DynLazySpan<'db>,
    ) -> Self {
        Self {
            label,
            expr_prop,
            label_span,
            expr_span,
        }
    }
}
