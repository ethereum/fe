use hir::{
    hir_def::{kw, CallArg, ExprId, GenericArgListId},
    span::{expr::LazyCallArgListSpan, params::LazyGenericArgListSpan, DynLazySpan},
};
use if_chain::if_chain;

use super::TyChecker;
use crate::{
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, FuncBodyDiagAccumulator},
        func_def::FuncDef,
        ty_def::{TyBase, TyData, TyId},
        ty_lower::lower_generic_arg_list,
    },
    HirAnalysisDb,
};

pub(super) struct Callable {
    func_def: FuncDef,
    generic_args: Vec<TyId>,
}

impl Callable {
    pub(super) fn new(
        db: &dyn HirAnalysisDb,
        ty: TyId,
        span: DynLazySpan,
    ) -> Result<Self, FuncBodyDiag> {
        let (base, args) = ty.decompose_ty_app(db);

        if base.is_ty_var(db) {
            return Err(BodyDiag::TypeMustBeKnown(span).into());
        }

        let TyData::TyBase(TyBase::Func(func_def)) = base.data(db) else {
            return Err(BodyDiag::not_callable(db, span, ty).into());
        };

        let params = ty.generic_args(db);
        assert_eq!(params.len(), args.len());

        Ok(Self {
            func_def: *func_def,
            generic_args: args.to_vec(),
        })
    }

    pub(super) fn ret_ty(&self, db: &dyn HirAnalysisDb) -> TyId {
        self.func_def.ret_ty(db).instantiate(db, &self.generic_args)
    }

    pub(super) fn unify_generic_args(
        &mut self,
        tc: &mut TyChecker,
        args: GenericArgListId,
        span: LazyGenericArgListSpan,
    ) -> bool {
        let db = tc.db;
        let hir_db = db.as_hir_db();

        if !args.is_given(hir_db) {
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
            }
            .into();
            FuncBodyDiagAccumulator::push(db, diag);

            return false;
        }

        for (i, (&given, arg)) in given_args.iter().zip(current_args.iter_mut()).enumerate() {
            *arg = tc.equate_ty(given, *arg, span.arg(i).into());
        }

        true
    }

    pub(super) fn check_args(
        &self,
        tc: &mut TyChecker,
        args: &[CallArg],
        span: LazyCallArgListSpan,
        receiver: Option<(ExprId, TyId)>,
    ) {
        let db = tc.db;

        let expected_arity = self.func_def.arg_tys(db).len();
        let given_arity = if receiver.is_some() {
            args.len() + 1
        } else {
            args.len()
        };
        if given_arity != expected_arity {
            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                def_span: self.func_def.name_span(db),
                given: given_arity,
                expected: expected_arity,
            }
            .into();
            FuncBodyDiagAccumulator::push(db, diag);
            return;
        }

        let mut expected_args = self.func_def.arg_tys(db).iter();
        if let Some(receiver) = receiver {
            let expected = expected_args
                .next()
                .unwrap()
                .instantiate(db, &self.generic_args);
            tc.equate_ty(receiver.1, expected, receiver.0.lazy_span(tc.body()).into());
        };

        for (i, (given, expected)) in args.iter().zip(expected_args).enumerate() {
            if_chain! {
                if let Some(expected_label) = self.func_def.param_label(db, i);
                let given_label = given.label_eagerly(db.as_hir_db(), tc.body());
                if expected_label != kw::SELF;
                if Some(expected_label) != given_label;
                then {
                    let primary = if given.label.is_some() {
                        span.arg(i).label().into()
                    } else {
                        span.arg(i).into()
                    };
                    let diag = BodyDiag::CallArgLabelMismatch {
                        primary,
                        def_span: self.func_def.name_span(db),
                        given: given_label,
                        expected: expected_label,
                    }
                    .into();
                    FuncBodyDiagAccumulator::push(db, diag);
                }
            }

            let expected = expected.instantiate(db, &self.generic_args);
            let given_ty = tc.fresh_ty();
            let given_ty = tc.check_expr(given.expr, given_ty).ty;
            tc.equate_ty(given_ty, expected, span.arg(i).expr().into());
        }
    }
}
