use num_bigint::{BigInt, BigUint};

use crate::HirAnalysisDb;

use super::ty_def::{TyId, TyParam, TyVar};

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
            DependentTyData::TyLit(lit) => lit.pretty_print(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependentTyData {
    TyVar(TyVar),
    TyParam(TyParam),
    TyLit(TyLit),
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
