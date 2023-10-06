use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    /// Functions for import/name resolutions.
    name_resolution::resolve_path_early_impl,
    name_resolution::resolve_imports,
    name_resolution::diagnostics::NameResolutionDiagAccumulator,
    name_resolution::diagnostics::ImportResolutionDiagAccumulator,
    /// Type system.
    ty::ty_def::TyId,
    ty::ty_def::ty_kind,
    ty::ty_def::free_inference_keys,
    ty::ty_def::pretty_print_ty,
    ty::ty_def::AdtDef,
    ty::ty_def::AdtRefId,
    /// Type lowering.
    ty::ty_lower::lower_hir_ty,
    ty::ty_lower::lower_adt,
    ty::ty_lower::lower_type_alias,
    ty::ty_lower::collect_generic_params,
    ty::ty_lower::GenericParamOwnerId,
    /// Trait lowering.
    ty::trait_lower::lower_trait,
    ty::trait_lower::lower_trait_ref,
    ty::trait_lower::collect_trait_impls,
    /// Item Definition analysis.
    ty::def_analysis::check_recursive_adt,
    ty::def_analysis::analyze_adt,
    ty::def_analysis::analyze_type_alias,
    ty::def_analysis::analyze_trait,
    /// Trait system.
    ty::trait_::TraitDef,
    ty::trait_::TraitInstId,
    ty::trait_::Implementor,
    ty::trait_::ingot_trait_env,
    ty::trait_::trait_implementors,
    ty::constraint::collect_super_traits,
    ty::constraint::collect_trait_constraints,
    ty::constraint::collect_adt_constraints,
    ty::constraint::collect_implementor_constraints,
    ty::constraint::super_trait_insts,
    ty::constraint::compute_super_assumptions,
    ty::constraint::ty_constraints,
    ty::constraint::trait_inst_constraints,
    ty::constraint::PredicateId,
    ty::constraint::PredicateListId,
    ty::constraint_solver::is_goal_satisfiable,
    ty::constraint_solver::check_ty_sat,
    ty::constraint_solver::check_trait_inst_sat,
    /// Diagnostic accumulators.
    ty::diagnostics::AdtDefDiagAccumulator,
    ty::diagnostics::TypeAliasDefDiagAccumulator,
    ty::diagnostics::TraitDefDiagAccumulator,
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
