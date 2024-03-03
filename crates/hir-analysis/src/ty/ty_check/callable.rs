use hir::{
    hir_def::{CallArg, GenericArgListId},
    span::{expr::LazyCallArgListSpan, params::LazyGenericArgListSpan, DynLazySpan},
};
use if_chain::if_chain;
use rustc_hash::FxHashMap;

use super::TyChecker;
use crate::{
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, FuncBodyDiagAccumulator},
        ty_def::{FuncDef, TyBase, TyData, TyId},
        ty_lower::lower_generic_arg_list,
    },
    HirAnalysisDb,
};

pub(super) struct Callable {
    func_def: FuncDef,
    subst: FxHashMap<TyId, TyId>,
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

        let subst = ty
            .generic_params(db)
            .iter()
            .copied()
            .zip(args.iter().copied())
            .collect();

        Ok(Self {
            func_def: *func_def,
            subst,
        })
    }

    pub(super) fn ret_ty(&mut self, db: &dyn HirAnalysisDb) -> TyId {
        self.func_def.ret_ty(db).apply_subst(db, &mut self.subst)
    }

    pub(super) fn apply_generic_args(
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
        let params = self.func_def.original_params(db);
        if params.len() != given_args.len() {
            let diag = BodyDiag::CallGenericArgNumMismatch {
                primary: span.into(),
                func: self.func_def.hir_func(db),
                given: given_args.len(),
                expected: params.len(),
            }
            .into();
            FuncBodyDiagAccumulator::push(db, diag);
            return false;
        }

        for (i, (&given, param)) in given_args.iter().zip(params.iter()).enumerate() {
            let expected = self.subst.get_mut(param).unwrap();
            *expected = tc.equate_ty(given, *expected, span.arg(i).into());
        }

        true
    }

    pub(super) fn check_args(
        &mut self,
        tc: &mut TyChecker,
        args: &[CallArg],
        span: LazyCallArgListSpan,
    ) {
        let db = tc.db;
        let hir_db = db.as_hir_db();

        let Some(params) = self
            .func_def
            .hir_func(db)
            .params(hir_db)
            .to_opt()
            .map(|param| param.data(hir_db))
        else {
            return;
        };

        if args.len() != params.len() {
            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                func: self.func_def.hir_func(db),
                given: args.len(),
                expected: params.len(),
            }
            .into();
            FuncBodyDiagAccumulator::push(db, diag);
            return;
        }

        let expected_args = self.func_def.arg_tys(db);
        for (i, (given, expected)) in args.iter().zip(expected_args.iter()).enumerate() {
            if_chain! {
                if let Some(expected_label) = params[i].label_eagerly();
                if Some(expected_label) != given.label_eagerly(hir_db, tc.body());
                then {
                    let primary = if given.label.is_some() {
                        span.arg(i).label().into()
                    } else {
                        span.arg(i).into()
                    };
                    let diag = BodyDiag::CallArgLabelMismatch {
                        primary,
                        func: self.func_def.hir_func(db),
                        given: given.label_eagerly(hir_db, tc.body()),
                        expected: expected_label,
                    }
                    .into();
                    FuncBodyDiagAccumulator::push(db, diag);
                }
            }

            let expected = expected.apply_subst(db, &mut self.subst);
            tc.check_expr(given.expr, expected);
        }
    }
}
