use num_bigint::{BigInt, BigUint};

use super::ty_def::{TyId, TyParam, TyVar};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependentTy {
    pub ty: TyId,
    pub data: DependentTyData,
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
