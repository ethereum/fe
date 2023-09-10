use hir::span::DynLazySpan;

#[salsa::accumulator]
pub struct StructDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct EnumDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct ContractDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyLowerDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    AssocTy(DynLazySpan),
    InvalidType(DynLazySpan),
    NotFullyAppliedType(DynLazySpan),
}

impl TyLowerDiag {
    pub(super) fn assoc_ty(span: impl Into<DynLazySpan>) -> Self {
        Self::AssocTy(span.into())
    }

    pub(super) fn invalid_type(span: impl Into<DynLazySpan>) -> Self {
        Self::InvalidType(span.into())
    }

    pub fn not_fully_applied_type(span: impl Into<DynLazySpan>) -> Self {
        Self::NotFullyAppliedType(span.into())
    }
}
