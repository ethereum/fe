use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    /// Functions for import/name resolutions.
    name_resolution::resolve_path_early_impl,
    name_resolution::resolve_imports,
    name_resolution::diagnostics::NameResolutionDiagAccumulator,
    name_resolution::diagnostics::ImportResolutionDiagAccumulator,
    /// Type inference.
    ty::ty::TyId,
    ty::ty::ty_kind,
    ty::ty::AdtDef,
    ty::ty::AdtRefId,
    /// Type lowering.
    ty::lower::lower_hir_ty,
    ty::lower::lower_adt,
    ty::lower::lower_type_alias,
    /// ADT analysis.
    ty::adt_analysis::check_recursive_adt,
    ty::adt_analysis::analyze_adt,
    // Trait resolution.
    ty::trait_::TraitDef,
    ty::trait_::TraitInstId,
    ty::diagnostics::AdtDefDiagAccumulator,
    ty::diagnostics::TypeAliasDefDiagAccumulator,
);

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + HirDb {
    fn as_hir_analysis_db(&self) -> &dyn HirAnalysisDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + HirDb {}

pub mod name_resolution;
pub mod ty;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: DynLazySpan,
}
