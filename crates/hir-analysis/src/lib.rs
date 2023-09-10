use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    /// Functions for import/name resolutions.
    name_resolution::resolve_path_early_impl,
    name_resolution::resolve_imports,
    name_resolution::diagnostics::NameResolutionDiagAccumulator,
    name_resolution::diagnostics::ImportResolutionDiagAccumulator,
    /// Type inference.
    infer::ty::TyId,
    infer::ty::ty_kind,
    infer::ty::AdtDef,
    /// Type lowering.
    infer::lower::lower_enum,
    infer::lower::lower_struct,
    infer::lower::lower_contract,
    infer::lower::lower_type_alias,
    // Trait resolution.
    infer::trait_::TraitDef,
    infer::trait_::TraitInstId,
    infer::diagnostics::StructDefDiagAccumulator,
    infer::diagnostics::EnumDefDiagAccumulator,
    infer::diagnostics::TypeAliasDefDiagAccumulator,
    infer::diagnostics::ContractDefDiagAccumulator,
);

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + HirDb {
    fn as_hir_analysis_db(&self) -> &dyn HirAnalysisDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + HirDb {}

pub mod infer;
pub mod name_resolution;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: DynLazySpan,
}
