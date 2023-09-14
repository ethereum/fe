use hir::{
    hir_def::{scope_graph::ScopeId, TypeId as HirTyId},
    span::DynLazySpan,
    visitor::prelude::{walk_ty as hir_walk_ty, *},
};

use crate::HirAnalysisDb;

use super::{
    diagnostics::TyLowerDiag,
    lower::lower_hir_ty,
    ty::{AdtDef, InvalidCause, PrimTy, TyConcrete, TyData, TyId, TyParam, TyVar},
};

pub trait TyVisitor {
    fn visit_ty(&mut self, db: &dyn HirAnalysisDb, ty: TyId) {
        walk_ty(self, db, ty)
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, db: &dyn HirAnalysisDb, var: &TyVar) {}

    #[allow(unused_variables)]
    fn visit_param(&self, db: &dyn HirAnalysisDb, ty_param: &TyParam) {}

    fn visit_app(&mut self, db: &dyn HirAnalysisDb, abs: TyId, arg: TyId) {
        self.visit_ty(db, abs);
        self.visit_ty(db, arg);
    }

    #[allow(unused_variables)]
    fn visit_ty_con(&mut self, db: &dyn HirAnalysisDb, ty_con: &TyConcrete) {
        walk_ty_con(self, db, ty_con);
    }

    #[allow(unused_variables)]
    fn visit_invalid(&mut self, db: &dyn HirAnalysisDb, cause: &InvalidCause) {}

    #[allow(unused_variables)]
    fn visit_prim(&mut self, db: &dyn HirAnalysisDb, prim: &PrimTy) {}

    #[allow(unused_variables)]
    fn visit_adt(&mut self, db: &dyn HirAnalysisDb, adt: AdtDef) {}
}

pub fn walk_ty<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty: TyId)
where
    V: TyVisitor + ?Sized,
{
    match ty.data(db) {
        TyData::TyVar(var) => visitor.visit_var(db, &var),

        TyData::TyParam(param) => visitor.visit_param(db, &param),

        TyData::TyApp(abs, arg) => visitor.visit_app(db, abs, arg),

        TyData::TyCon(ty_con) => visitor.visit_ty_con(db, &ty_con),

        TyData::Invalid(cause) => visitor.visit_invalid(db, &cause),
    }
}

pub fn walk_ty_con<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty_con: &TyConcrete)
where
    V: TyVisitor + ?Sized,
{
    match ty_con {
        TyConcrete::Prim(prim) => visitor.visit_prim(db, prim),
        TyConcrete::Adt(adt) => visitor.visit_adt(db, *adt),
        TyConcrete::Abs => {}
    }
}

pub(super) struct TyDiagCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyLowerDiag>,
    scope: ScopeId,
}

impl<'db> TyDiagCollector<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId) -> Self {
        Self {
            db,
            diags: Vec::new(),
            scope,
        }
    }

    pub(super) fn collect(mut self, hir_ty: HirTyId, span: LazyTySpan) -> Vec<TyLowerDiag> {
        let mut ctxt = VisitorCtxt::new(self.db.as_hir_db(), span);
        self.visit_ty(&mut ctxt, hir_ty);
        self.diags
    }

    fn store_diag(&mut self, cause: InvalidCause, span: DynLazySpan) {
        match cause {
            InvalidCause::NotFullyApplied => {
                let diag = TyLowerDiag::not_fully_applied_type(span);
                self.diags.push(diag);
            }

            InvalidCause::KindMismatch { expected, given } => {
                let diag = TyLowerDiag::kind_mismatch(span, expected, given);
                self.diags.push(diag);
            }

            InvalidCause::TypeAliasArgumentMismatch {
                alias,
                n_given_args: n_given_arg,
            } => {
                let diag = TyLowerDiag::type_alias_argument_mismatch(span, alias, n_given_arg);
                self.diags.push(diag);
            }

            InvalidCause::AssocTy => {
                let diag = TyLowerDiag::assoc_ty(span);
                self.diags.push(diag);
            }

            // NOTE: We can `InvalidCause::Other` because it's already reported by other passes.
            InvalidCause::Other => {}
        }
    }
}

impl<'db> Visitor for TyDiagCollector<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        match ty.data(self.db) {
            TyData::Invalid(cause) => {
                self.store_diag(cause, ctxt.span().unwrap().into());
            }

            TyData::TyApp(lhs, arg) => {
                let mut args = vec![];
                ty_arg_lexical_order(self.db, &mut args, lhs, arg);
                for (idx, arg) in args.into_iter().enumerate() {
                    match arg.data(self.db) {
                        TyData::Invalid(cause @ InvalidCause::KindMismatch { .. }) => {
                            let span = ty_args_span(self.db, hir_ty, ctxt.span().unwrap(), idx);
                            self.store_diag(cause, span);
                            return;
                        }

                        _ => {}
                    }
                }
            }

            _ => {}
        }

        hir_walk_ty(self, ctxt, hir_ty);
    }
}

/// Returns `TyApp` arguments in recursive order.
/// e.g.,
/// `TyApp(TyApp(T, A1), TyApp(U, A2))` returns `[A1, TyApp(U, A2]`.
fn ty_arg_lexical_order(db: &dyn HirAnalysisDb, args: &mut Vec<TyId>, lhs: TyId, arg: TyId) {
    match lhs.data(db) {
        TyData::TyApp(deep_lhs, deep_arg) => ty_arg_lexical_order(db, args, deep_lhs, deep_arg),
        _ => {}
    }

    args.push(arg)
}

fn ty_args_span(
    db: &dyn HirAnalysisDb,
    ty: HirTyId,
    ty_span: LazyTySpan,
    idx: usize,
) -> DynLazySpan {
    use hir::hir_def::TypeKind as HirTypeKind;
    match ty.data(db.as_hir_db()) {
        HirTypeKind::Ptr(_) => {
            if idx == 0 {
                ty_span.into_ptr_type().pointee().into()
            } else {
                DynLazySpan::invalid()
            }
        }

        HirTypeKind::Path(..) => ty_span
            .into_path_type()
            .generic_args_moved()
            .arg_moved(idx)
            .into(),

        HirTypeKind::Tuple(_) => ty_span.into_tuple_type().elem_ty_moved(idx).into(),

        HirTypeKind::Array(..) => {
            let span = ty_span.into_array_type();
            if idx == 0 {
                span.elem_moved().into()
            } else if idx == 1 {
                span.len_moved().into()
            } else {
                DynLazySpan::invalid()
            }
        }

        HirTypeKind::SelfType => {
            // TODO: Generic args.

            DynLazySpan::invalid()
        }
    }
}
