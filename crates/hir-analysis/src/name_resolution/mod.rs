pub mod diagnostics;

mod import_resolver;
mod name_resolver;
mod path_resolver;
mod visibility_checker;

pub use import_resolver::ResolvedImports;
pub use name_resolver::{
    NameBinding, NameDerivation, NameDomain, NameQuery, NameRes, QueryDirective,
};

use hir::{
    analysis_pass::ModuleAnalysisPass,
    diagnostics::DiagnosticVoucher,
    hir_def::{
        scope_graph::ScopeId, FieldDefListId, GenericParamListId, IngotId, ItemKind, TopLevelMod,
        VariantDefListId,
    },
    span::item::{LazyFieldDefListSpan, LazyItemSpan, LazyVariantDefListSpan},
    visitor::{
        walk_field_def_list, walk_generic_param_list, walk_item, walk_variant_def_list, Visitor,
        VisitorCtxt,
    },
};
use rustc_hash::FxHashSet;

use crate::HirAnalysisDb;

use self::{
    diagnostics::{NameResDiag, NameResolutionDiagAccumulator},
    import_resolver::DefaultImporter,
    name_resolver::{NameResolutionError, ResolvedQueryCacheStore},
};

pub struct ImportAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ImportAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }

    pub fn resolve_imports(&self, ingot: IngotId) -> &'db ResolvedImports {
        resolve_imports(self.db, ingot)
    }
}

impl<'db> ModuleAnalysisPass for ImportAnalysisPass<'db> {
    fn run_on_module(&mut self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>> {
        let ingot = top_mod.ingot(self.db.as_hir_db());
        resolve_imports::accumulated::<NameResolutionDiagAccumulator>(self.db, ingot)
            .into_iter()
            .filter_map(|diag| (diag.top_mod(self.db) == top_mod).then(|| Box::new(diag) as _))
            .collect()
    }
}

pub struct DefConflictAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> DefConflictAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for DefConflictAnalysisPass<'db> {
    fn run_on_module(&mut self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>> {
        let errors =
            resolve_path_early::accumulated::<NameResolutionDiagAccumulator>(self.db, top_mod);

        // TODO: Impl collector.
        errors
            .into_iter()
            .filter_map(|err| matches!(err, NameResDiag::Conflict(..)).then(|| Box::new(err) as _))
            .collect()
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn resolve_imports(db: &dyn HirAnalysisDb, ingot: IngotId) -> ResolvedImports {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, diags) = resolver.resolve_imports();
    for diag in diags {
        NameResolutionDiagAccumulator::push(db, diag);
    }

    imports
}

/// Performs early path resolution and cache the resolutions for paths appeared
/// in the given module. Also checks the conflict of the item definitions
///
/// NOTE: This method doesn't check the conflict in impl blocks since it
/// requires ingot granularity analysis.
#[salsa::tracked(return_ref)]
#[allow(unused)]
pub(crate) fn resolve_path_early(
    db: &dyn HirAnalysisDb,
    top_mod: TopLevelMod,
) -> ResolvedQueryCacheStore {
    let importer = DefaultImporter;
    let mut resolver = PathResolver::new(db, &importer);
    resolver.resolve_all(top_mod);

    for diag in resolver.diags {
        NameResolutionDiagAccumulator::push(db, diag);
    }
    resolver.inner.into_cache_store()
}

struct PathResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    inner: name_resolver::NameResolver<'db, 'a>,
    diags: Vec<diagnostics::NameResDiag>,
    item_stack: Vec<ItemKind>,

    /// The set of scopes that have already been conflicted to avoid duplicate
    /// diagnostics.
    already_conflicted: FxHashSet<ScopeId>,
}

impl<'db, 'a> PathResolver<'db, 'a> {
    fn new(db: &'db dyn HirAnalysisDb, importer: &'a DefaultImporter) -> Self {
        let resolver = name_resolver::NameResolver::new(db, importer);
        Self {
            db: db.as_hir_analysis_db(),
            inner: resolver,
            diags: Vec::new(),
            item_stack: Vec::new(),
            already_conflicted: FxHashSet::default(),
        }
    }

    fn resolve_all(&mut self, top_mod: TopLevelMod) {
        let mut ctxt = VisitorCtxt::with_item(self.db.as_hir_db(), top_mod.into());
        self.visit_item(&mut ctxt, top_mod.into());
    }

    fn check_item_conflict(&mut self, item: ItemKind) {
        let scope = ScopeId::from_item(item);
        self.check_conflict(scope);
    }

    fn check_field_conflict(&mut self, fields: FieldDefListId) {
        let parent_item = *self.item_stack.last().unwrap();
        for i in 0..fields.data(self.db.as_hir_db()).len() {
            let scope = ScopeId::Field(parent_item, i);
            self.check_conflict(scope);
        }
    }

    fn check_variant_conflict(&mut self, variants: VariantDefListId) {
        let parent_item = *self.item_stack.last().unwrap();
        for i in 0..variants.data(self.db.as_hir_db()).len() {
            let scope = ScopeId::Variant(parent_item, i);
            self.check_conflict(scope);
        }
    }

    fn check_generic_param_conflict(&mut self, params: GenericParamListId) {
        let parent_item = *self.item_stack.last().unwrap();
        for i in 0..params.data(self.db.as_hir_db()).len() {
            let scope = ScopeId::GenericParam(parent_item, i);
            self.check_conflict(scope);
        }
    }

    fn check_conflict(&mut self, scope: ScopeId) {
        if !self.already_conflicted.insert(scope) {
            return;
        }

        let Some(query) = self.make_query_for_conflict_check(scope) else {
            return;
        };

        let domain = NameDomain::from_scope(scope);
        let binding = self.inner.resolve_query(query);
        match binding.res_by_domain(domain) {
            Ok(_) => {}

            Err(NameResolutionError::Ambiguous(cands)) => {
                let conflicted_span = cands
                    .iter()
                    .filter_map(|res| {
                        let conflicted_scope = res.scope()?;
                        self.already_conflicted.insert(conflicted_scope);
                        conflicted_scope.name_span(self.db.as_hir_db())
                    })
                    .collect();

                let diag = diagnostics::NameResDiag::conflict(
                    scope.name(self.db.as_hir_db()).unwrap(),
                    conflicted_span,
                );
                self.diags.push(diag);
            }
            Err(_) => unreachable!(),
        };
    }

    fn make_query_for_conflict_check(&self, scope: ScopeId) -> Option<NameQuery> {
        let name = scope.name(self.db.as_hir_db())?;
        let mut directive = QueryDirective::new();
        directive.disallow_lex().disallow_glob().disallow_external();

        let parent_scope = scope.parent(self.db.as_hir_db())?;
        Some(NameQuery::with_directive(name, parent_scope, directive))
    }
}

impl<'db, 'a> Visitor for PathResolver<'db, 'a> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'_, LazyItemSpan>, item: ItemKind) {
        self.check_item_conflict(item);

        self.item_stack.push(item);
        walk_item(self, ctxt, item);
        self.item_stack.pop();
    }

    fn visit_field_def_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFieldDefListSpan>,
        field: FieldDefListId,
    ) {
        self.check_field_conflict(field);
        walk_field_def_list(self, ctxt, field);
    }

    fn visit_variant_def_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefListSpan>,
        variant: VariantDefListId,
    ) {
        self.check_variant_conflict(variant);
        walk_variant_def_list(self, ctxt, variant);
    }

    fn visit_generic_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, hir::span::params::LazyGenericParamListSpan>,
        params: GenericParamListId,
    ) {
        self.check_generic_param_conflict(params);
        walk_generic_param_list(self, ctxt, params);
    }
}
