use hir::hir_def::{Body, Expr, IntegerId, LitKind, Partial};

use super::{
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    unify::UnificationTable,
};
use crate::{
    ty::ty_def::{Kind, TyBase, TyData, TyVarUniverse},
    HirAnalysisDb,
};

#[salsa::interned]
pub struct DependentTyId {
    #[return_ref]
    pub(super) data: DependentTyData,
}

#[salsa::tracked]
pub(crate) fn evaluate_dependent_ty(
    db: &dyn HirAnalysisDb,
    dependent_ty: DependentTyId,
    expected_ty: TyId,
) -> DependentTyId {
    let DependentTyData::UnEvaluated(body) = dependent_ty.data(db) else {
        return dependent_ty;
    };

    let Partial::Present(expr) = body.expr(db.as_hir_db()).data(db.as_hir_db(), *body) else {
        let data = DependentTyData::Evaluated(
            ResolvedDependentTy::Invalid,
            TyId::invalid(db, InvalidCause::Other),
        );
        return DependentTyId::new(db, data);
    };

    let mut table = UnificationTable::new(db);
    let (resolved, ty) = match expr {
        Expr::Lit(LitKind::Bool(b)) => (
            ResolvedDependentTy::LitBool(*b),
            TyId::new(db, TyData::TyBase(TyBase::bool())),
        ),

        Expr::Lit(LitKind::Int(i)) => (
            ResolvedDependentTy::LitInt(*i),
            table.new_var(TyVarUniverse::Integral, &Kind::Star),
        ),

        _ => todo!(),
    };

    if ty.is_invalid(db) {
        let resolved = ResolvedDependentTy::Invalid;
        let data = DependentTyData::Evaluated(resolved, TyId::invalid(db, InvalidCause::Other));
        return DependentTyId::new(db, data);
    }

    if table.unify(expected_ty, ty) {
        let data = DependentTyData::Evaluated(resolved, expected_ty);
        DependentTyId::new(db, data)
    } else {
        let resolved = ResolvedDependentTy::Invalid;
        let invalid = InvalidCause::DependentTyMismatch {
            expected: expected_ty,
            given: ty,
        };

        let data = DependentTyData::Evaluated(resolved, TyId::invalid(db, invalid));
        DependentTyId::new(db, data)
    }
}

impl DependentTyId {
    #[allow(unused)]
    pub fn ty(self, db: &dyn HirAnalysisDb) -> TyId {
        match self.data(db) {
            DependentTyData::TyVar(_, ty) => *ty,
            DependentTyData::TyParam(_, ty) => *ty,
            DependentTyData::Evaluated(_, ty) => *ty,
            DependentTyData::UnEvaluated(_) => TyId::invalid(db, InvalidCause::Other),
        }
    }

    pub(super) fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        match &self.data(db) {
            DependentTyData::TyVar(var, _) => var.pretty_print(),
            DependentTyData::TyParam(param, _) => param.pretty_print(db),
            DependentTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
            DependentTyData::UnEvaluated(_) => "<unevaluated>".to_string(),
        }
    }

    #[allow(unused)]
    pub(super) fn evaluate(self, db: &dyn HirAnalysisDb, ty: TyId) -> Self {
        evaluate_dependent_ty(db, self, ty)
    }

    pub(super) fn from_body(db: &dyn HirAnalysisDb, body: Body) -> Self {
        let data = DependentTyData::UnEvaluated(body);
        Self::new(db, data)
    }

    pub(super) fn from_opt_body(db: &dyn HirAnalysisDb, body: Partial<Body>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body),
            Partial::Absent => {
                let resolved = ResolvedDependentTy::Invalid;
                let ty = TyId::invalid(db, InvalidCause::Other);
                let data = DependentTyData::Evaluated(resolved, ty);
                Self::new(db, data)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependentTyData {
    TyVar(TyVar, TyId),
    TyParam(TyParam, TyId),
    Evaluated(ResolvedDependentTy, TyId),
    UnEvaluated(Body),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedDependentTy {
    LitInt(IntegerId),
    LitBool(bool),
    Invalid,
}

impl ResolvedDependentTy {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            ResolvedDependentTy::LitInt(val) => format!("{}", val.data(db.as_hir_db())),
            ResolvedDependentTy::LitBool(val) => format!("{}", val),
            ResolvedDependentTy::Invalid => "<invalid>".to_string(),
        }
    }
}
