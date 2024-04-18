use hir::{
    hir_def::{kw, GenericArgListId, IdentId},
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
    args: Vec<TyId>,
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
            args: args.to_vec(),
        })
    }

    pub(super) fn ret_ty(&self, db: &dyn HirAnalysisDb) -> TyId {
        self.func_def.ret_ty(db).instantiate(db, &self.args)
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
        let current_args = &mut self.args[offset..];

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
        args: &[(Option<IdentId>, TyId)],
        span: LazyCallArgListSpan,
        receiver_span: Option<DynLazySpan>,
    ) {
        let db = tc.db;

        let expected_arity = self.func_def.arg_tys(db).len();
        if args.len() != expected_arity {
            let (given, expected) = if receiver_span.is_some() {
                (args.len() - 1, expected_arity - 1)
            } else {
                (args.len(), expected_arity)
            };

            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                def_span: self.func_def.name_span(db),
                given,
                expected,
            }
            .into();
            FuncBodyDiagAccumulator::push(db, diag);
            return;
        }

        let expected_args = self.func_def.arg_tys(db);
        for (i, (given, expected)) in args.iter().zip(expected_args.iter()).enumerate() {
            if_chain! {
                if let Some(expected_label) = self.func_def.param_label(db, i);
                if expected_label != kw::SELF;
                if Some(expected_label) != given.0;
                let idx = if receiver_span.is_some() {
                    i - 1
                } else {
                    i
                };
                then {
                    let primary = if given.0.is_some() {
                        span.arg(idx).label().into()
                    } else {
                        span.arg(idx).into()
                    };
                    let diag = BodyDiag::CallArgLabelMismatch {
                        primary,
                        def_span: self.func_def.name_span(db),
                        given: given.0,
                        expected: expected_label,
                    }
                    .into();
                    FuncBodyDiagAccumulator::push(db, diag);
                }
            }

            let expected = expected.instantiate(db, &self.args);
            let span = if let Some(receiver_span) = &receiver_span {
                if i == 0 {
                    receiver_span.clone()
                } else {
                    span.arg(i - 1).expr().into()
                }
            } else {
                span.arg(i).expr().into()
            };
            tc.equate_ty(given.1, expected, span);
        }
    }
}
