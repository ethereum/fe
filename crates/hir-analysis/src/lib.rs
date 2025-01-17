use hir::{span::DynLazySpan, HirDb};

#[salsa::db]
pub trait HirAnalysisDb: salsa::Database + HirDb {
    fn as_hir_analysis_db(&self) -> &dyn HirAnalysisDb;
}

pub mod name_resolution;
pub mod ty;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<'db, T> {
    pub data: T,
    pub span: DynLazySpan<'db>,
}
