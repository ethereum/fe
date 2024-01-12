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
    expected_ty: Option<TyId>,
) -> DependentTyId {
    let DependentTyData::UnEvaluated(body) = dependent_ty.data(db) else {
        let dep_ty_ty = dependent_ty.ty(db);
        return match check_dependent_ty(db, dep_ty_ty, expected_ty, &mut UnificationTable::new(db))
        {
            Ok(_) => dependent_ty,
            Err(cause) => {
                let ty = TyId::invalid(db, cause);
                return dependent_ty.swap_ty(db, ty);
            }
        };
    };

    let Partial::Present(expr) = body.expr(db.as_hir_db()).data(db.as_hir_db(), *body) else {
        let data = DependentTyData::Evaluated(
            EvaluatedDependentTy::Invalid,
            TyId::invalid(db, InvalidCause::Other),
        );
        return DependentTyId::new(db, data);
    };

    let mut table = UnificationTable::new(db);
    let (resolved, ty) = match expr {
        Expr::Lit(LitKind::Bool(b)) => (
            EvaluatedDependentTy::LitBool(*b),
            TyId::new(db, TyData::TyBase(TyBase::bool())),
        ),

        Expr::Lit(LitKind::Int(i)) => (
            EvaluatedDependentTy::LitInt(*i),
            table.new_var(TyVarUniverse::Integral, &Kind::Star),
        ),

        _ => {
            return DependentTyId::new(
                db,
                DependentTyData::Evaluated(
                    EvaluatedDependentTy::Invalid,
                    TyId::invalid(db, InvalidCause::InvalidDependentTyExpr { body: *body }),
                ),
            );
        }
    };

    let data = match check_dependent_ty(db, ty, expected_ty, &mut table) {
        Ok(ty) => DependentTyData::Evaluated(resolved, ty),
        Err(err) => DependentTyData::Evaluated(resolved, TyId::invalid(db, err)),
    };

    DependentTyId::new(db, data)
}

// FIXME: When we add type inference, we need to use the inference engine to
// check the type of the expression instead of this function.
fn check_dependent_ty(
    db: &dyn HirAnalysisDb,
    dep_ty_ty: TyId,
    expected_ty: Option<TyId>,
    table: &mut UnificationTable,
) -> Result<TyId, InvalidCause> {
    if dep_ty_ty.is_invalid(db) {
        return Err(InvalidCause::Other);
    }

    let Some(expected_ty) = expected_ty else {
        return Ok(dep_ty_ty);
    };

    if table.unify(expected_ty, dep_ty_ty) {
        Ok(expected_ty)
    } else {
        let invalid = InvalidCause::DependentTyMismatch {
            expected: expected_ty,
            given: dep_ty_ty,
        };
        Err(invalid)
    }
}

impl DependentTyId {
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
            DependentTyData::TyParam(param, ty) => {
                format!("const {}: {}", param.pretty_print(db), ty.pretty_print(db))
            }
            DependentTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
            DependentTyData::UnEvaluated(_) => "<unevaluated>".to_string(),
        }
    }

    pub(super) fn evaluate(self, db: &dyn HirAnalysisDb, expected_ty: Option<TyId>) -> Self {
        evaluate_dependent_ty(db, self, expected_ty)
    }

    pub(super) fn from_body(db: &dyn HirAnalysisDb, body: Body) -> Self {
        let data = DependentTyData::UnEvaluated(body);
        Self::new(db, data)
    }

    pub(super) fn from_opt_body(db: &dyn HirAnalysisDb, body: Partial<Body>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body),
            Partial::Absent => {
                let resolved = EvaluatedDependentTy::Invalid;
                let ty = TyId::invalid(db, InvalidCause::Other);
                let data = DependentTyData::Evaluated(resolved, ty);
                Self::new(db, data)
            }
        }
    }

    fn swap_ty(self, db: &dyn HirAnalysisDb, ty: TyId) -> Self {
        let data = match self.data(db) {
            DependentTyData::TyVar(var, _) => DependentTyData::TyVar(var.clone(), ty),
            DependentTyData::TyParam(param, _) => DependentTyData::TyParam(param.clone(), ty),
            DependentTyData::Evaluated(evaluated, _) => {
                DependentTyData::Evaluated(evaluated.clone(), ty)
            }
            DependentTyData::UnEvaluated(_) => {
                return self;
            }
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependentTyData {
    TyVar(TyVar, TyId),
    TyParam(TyParam, TyId),
    Evaluated(EvaluatedDependentTy, TyId),
    UnEvaluated(Body),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedDependentTy {
    LitInt(IntegerId),
    LitBool(bool),
    Invalid,
}

impl EvaluatedDependentTy {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            EvaluatedDependentTy::LitInt(val) => {
                format!("{}", val.data(db.as_hir_db()))
            }
            EvaluatedDependentTy::LitBool(val) => format!("{}", val),
            EvaluatedDependentTy::Invalid => "<invalid>".to_string(),
        }
    }
}
