use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    name_resolution::resolve_imports,
    name_resolution::diagnostics::ImportErrorAccumulator,
);

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + HirDb {
    fn as_hir_db(&self) -> &dyn HirDb {
        <Self as salsa::DbWithJar<hir::Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + HirDb {}

pub mod name_resolution;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: DynLazySpan,
}
