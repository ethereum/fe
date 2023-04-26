use common::db::Upcast;
use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar();

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + Upcast<dyn HirDb> {}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + Upcast<dyn HirDb> {}

pub mod name_resolution;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: DynLazySpan,
}
