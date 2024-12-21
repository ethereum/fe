use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    name_resolution::resolve_imports,
    name_resolution::traits_in_scope::available_traits_in_scope_impl,
    name_resolution::traits_in_scope::TraitScope<'_>,
    name_resolution::EarlyNameQueryId<'_>,
    name_resolution::resolve_query,
    // Type system.
    ty::ty_def::TyId<'_>,
    ty::ty_def::ty_kind,
    ty::ty_def::pretty_print_ty,
    ty::ty_def::decompose_ty_app,
    ty::ty_def::ty_flags,
    // Adt types.
    ty::adt_def::AdtDef<'_>,
    ty::adt_def::AdtRefId<'_>,
    ty::adt_def::lower_adt,
    // Func type.
    ty::func_def::FuncDef<'_>,
    ty::func_def::lower_func,
    // Const types.
    ty::const_ty::ConstTyId<'_>,
    ty::const_ty::evaluate_const_ty,
    // Type lowering.
    ty::ty_lower::lower_hir_ty,
    ty::ty_lower::lower_type_alias,
    ty::ty_lower::collect_generic_params,
    ty::ty_lower::GenericParamOwnerId<'_>,
    ty::ty_lower::GenericParamTypeSet<'_>,
    ty::ty_lower::evaluate_params_precursor,
    // Trait lowering.
    ty::trait_lower::lower_trait,
    ty::trait_lower::lower_trait_ref,
    ty::trait_lower::collect_trait_impls,
    ty::trait_lower::lower_impl_trait,
    ty::trait_lower::collect_implementor_methods,
    // Method collection.
    ty::method_table::collect_methods,
    ty::method_table::probe_method,
    // Item Definition analysis.
    ty::def_analysis::check_recursive_adt,
    ty::def_analysis::analyze_adt,
    ty::def_analysis::analyze_type_alias,
    ty::def_analysis::analyze_trait,
    ty::def_analysis::analyze_impl,
    ty::def_analysis::analyze_impl_trait,
    ty::def_analysis::analyze_func,
    // Trait system.
    ty::trait_def::TraitDef<'_>,
    ty::trait_def::TraitInstId<'_>,
    ty::trait_def::Implementor<'_>,
    ty::trait_def::ingot_trait_env,
    ty::trait_def::impls_for_trait,
    ty::trait_def::impls_for_ty,
    // Trait constraints
    ty::trait_resolution::constraint::collect_super_traits,
    ty::trait_resolution::constraint::collect_trait_constraints,
    ty::trait_resolution::constraint::collect_adt_constraints,
    ty::trait_resolution::constraint::collect_implementor_constraints,
    ty::trait_resolution::constraint::collect_impl_block_constraints,
    ty::trait_resolution::constraint::collect_func_def_constraints,
    ty::trait_resolution::constraint::collect_func_def_constraints_impl,
    ty::trait_resolution::constraint::ty_constraints,
    ty::trait_resolution::PredicateListId<'_>,
    ty::trait_resolution::is_goal_satisfiable,
    ty::trait_resolution::check_ty_wf,
    ty::trait_resolution::check_trait_inst_wf,
    ty::trait_resolution::ty_depth_impl,
    // Type checking.
    ty::ty_check::check_func_body,
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
pub struct Spanned<'db, T> {
    pub data: T,
    pub span: DynLazySpan<'db>,
}
