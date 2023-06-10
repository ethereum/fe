use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDefListId, FuncParamListId, GenericParamListId, IngotId,
        ItemKind, TopLevelMod, VariantDefListId,
    },
    span::{
        item::{LazyFieldDefListSpan, LazyItemSpan, LazyVariantDefListSpan},
        params::LazyFuncParamListSpan,
    },
    visitor::{
        walk_field_def_list, walk_func_param_list, walk_generic_param_list, walk_item,
        walk_variant_def_list, Visitor, VisitorCtxt,
    },
};

use crate::HirAnalysisDb;

use self::{
    diagnostics::NameResolutionDiagAccumulator,
    import_resolver::{DefaultImporter, ResolvedImports},
    name_resolver::{
        NameDomain, NameQuery, NameResolutionError, QueryDirective, ResolvedQueryCacheStore,
    },
};

pub mod diagnostics;
pub mod import_resolver;
pub mod name_resolver;
pub mod visibility_checker;

#[salsa::tracked(return_ref)]
pub fn resolve_imports(db: &dyn HirAnalysisDb, ingot: IngotId) -> ResolvedImports {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, diags) = resolver.resolve_imports();
    for diag in diags {
        NameResolutionDiagAccumulator::push(db, diag);
    }

    imports
}

pub fn resolve_imports_with_diag(
    db: &dyn HirAnalysisDb,
    ingot: IngotId,
) -> (&ResolvedImports, Vec<diagnostics::NameResolutionDiag>) {
    let imports = resolve_imports(db, ingot);
    let diagnostics = resolve_imports::accumulated::<NameResolutionDiagAccumulator>(db, ingot);
    (imports, diagnostics)
}

/// Performs early path resolution in the given module and checks the conflict
/// of the definitions.
#[salsa::tracked(return_ref)]
#[allow(unused)]
pub(crate) fn resolve_path_early(
    db: &dyn HirAnalysisDb,
    top_mod: TopLevelMod,
) -> ResolvedQueryCacheStore {
    let importer = DefaultImporter;
    PathResolver::new(db, &importer).resolve_all(top_mod);
    todo!()
}

struct PathResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    resolver: name_resolver::NameResolver<'db, 'a>,
    diags: Vec<diagnostics::NameResolutionDiag>,
    item_stack: Vec<ItemKind>,
}

impl<'db, 'a> PathResolver<'db, 'a> {
    fn new(db: &'db dyn HirAnalysisDb, importer: &'a DefaultImporter) -> Self {
        let resolver = name_resolver::NameResolver::new(db, importer);
        Self {
            db: db.as_hir_analysis_db(),
            resolver,
            diags: Vec::new(),
            item_stack: Vec::new(),
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

    fn check_func_param_conflict(&mut self, params: FuncParamListId) {
        let parent_item = *self.item_stack.last().unwrap();
        for i in 0..params.data(self.db.as_hir_db()).len() {
            let scope = ScopeId::FuncParam(parent_item, i);
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
        let Some(query) = self.make_query_for_conflict_check(scope) else {
            return;
        };

        let domain = NameDomain::from_scope(scope);
        let binding = self.resolver.resolve_query(query);
        match binding.res_in_domain(domain) {
            Ok(_) => {}
            Err(NameResolutionError::Ambiguous(cands)) => {
                let conflicted_span = cands
                    .iter()
                    .find_map(|res| {
                        let conflicted_scope = res.scope()?;
                        if conflicted_scope == scope {
                            None
                        } else {
                            conflicted_scope.name_span(self.db.as_hir_db())
                        }
                    })
                    .unwrap();

                let diag = diagnostics::NameResolutionDiag::conflict(
                    scope.name_span(self.db.as_hir_db()).unwrap(),
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

    fn visit_func_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFuncParamListSpan>,
        params: FuncParamListId,
    ) {
        self.check_func_param_conflict(params);
        walk_func_param_list(self, ctxt, params)
    }
}
