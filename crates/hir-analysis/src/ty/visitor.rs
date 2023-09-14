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
    accumulated: Vec<TyLowerDiag>,
    scope: ScopeId,
}

impl<'db> TyDiagCollector<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId) -> Self {
        Self {
            db,
            accumulated: Vec::new(),
            scope,
        }
    }

    pub(super) fn collect(mut self, hir_ty: HirTyId, span: LazyTySpan) -> Vec<TyLowerDiag> {
        let mut ctxt = VisitorCtxt::new(self.db.as_hir_db(), span);
        self.visit_ty(&mut ctxt, hir_ty);
        self.accumulated
    }

    fn collect_impl(&mut self, cause: InvalidCause, span: LazyTySpan) {
        let span: DynLazySpan = span.into();
        match cause {
            InvalidCause::NotFullyApplied => {
                let diag = TyLowerDiag::not_fully_applied_type(span);
                self.accumulated.push(diag);
            }

            InvalidCause::TyAppFailed { abs, arg } => {
                let diag = TyLowerDiag::ty_app_failed(self.db, span, abs, arg);
                self.accumulated.push(diag);
            }

            InvalidCause::KindMismatch { expected, given } => {
                let diag = TyLowerDiag::kind_mismatch(self.db, span, expected, given);
                self.accumulated.push(diag);
            }

            InvalidCause::TypeAliasArgumentMismatch {
                alias,
                n_given_args: n_given_arg,
            } => {
                let diag = TyLowerDiag::type_alias_argument_mismatch(span, alias, n_given_arg);
                self.accumulated.push(diag);
            }

            InvalidCause::AssocTy => {
                let diag = TyLowerDiag::assoc_ty(span);
                self.accumulated.push(diag);
            }

            // NOTE: We can `InvalidCause::Other` because it's already reported by other passes.
            InvalidCause::Other => {}
        }
    }
}

impl<'db> Visitor for TyDiagCollector<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        if let Some(cause) = ty.invalid_cause(self.db) {
            self.collect_impl(cause, ctxt.span().unwrap());
        }

        hir_walk_ty(self, ctxt, hir_ty);
    }
}
