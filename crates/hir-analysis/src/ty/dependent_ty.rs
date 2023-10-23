use hir::hir_def::Body;
use num_bigint::{BigInt, BigUint};

use crate::HirAnalysisDb;

use super::ty_def::{InvalidCause, TyId, TyParam, TyVar};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependentTy {
    pub ty: TyId,
    pub data: DependentTyData,
}

impl DependentTy {
    pub(super) fn new(ty: TyId, data: DependentTyData) -> Self {
        Self { ty, data }
    }

    pub(super) fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match &self.data {
            DependentTyData::TyVar(var) => var.pretty_print(),
            DependentTyData::TyParam(param) => param.pretty_print(db),
            DependentTyData::TyExprNonEvaluated(_) => unreachable!(),
            DependentTyData::TyLit(lit) => lit.pretty_print(),
            DependentTyData::Invalid(_) => "<invalid>".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependentTyData {
    TyVar(TyVar),
    TyParam(TyParam),
    TyExprNonEvaluated(TyExpr),
    TyLit(TyLit),
    Invalid(InvalidCause),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyExpr {
    Unresolved(Body),
    Resolved(TyLit),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLit {
    LitUInt(BigUint),
    LitInt(BigInt),
    LitBool(bool),
}

impl TyLit {
    pub fn pretty_print(&self) -> String {
        match self {
            TyLit::LitUInt(val) => format!("{}", val),
            TyLit::LitInt(val) => format!("{}", val),
            TyLit::LitBool(val) => format!("{}", val),
        }
    }
}
