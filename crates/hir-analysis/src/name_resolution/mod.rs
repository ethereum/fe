pub mod diagnostics;

mod import_resolver;
mod name_resolver;
mod path_resolver;
mod visibility_checker;

use either::Either;
pub use import_resolver::ResolvedImports;
pub use name_resolver::{
    NameDerivation, NameDomain, NameQuery, NameRes, NameResBucket, NameResKind, QueryDirective,
};
pub use path_resolver::EarlyResolvedPath;

use hir::{
    analysis_pass::ModuleAnalysisPass,
    diagnostics::DiagnosticVoucher,
    hir_def::{
        scope_graph::ScopeId, Expr, ExprId, IdentId, IngotId, ItemKind, Partial, Pat, PatId,
        PathId, TopLevelMod, TypeBound, TypeId,
    },
    visitor::prelude::*,
};
use rustc_hash::FxHashSet;

use crate::HirAnalysisDb;

use self::{
    diagnostics::{ImportResolutionDiagAccumulator, NameResDiag, NameResolutionDiagAccumulator},
    import_resolver::DefaultImporter,
    name_resolver::{NameResolutionError, ResolvedQueryCacheStore},
    path_resolver::EarlyPathResolver,
};

// TODO: Implement `resolve_path` and `resolve_segments` after implementing the
// late path resolution.

/// Resolves the given path in the given scope.
/// It's not necessary to report any error even if the `EarlyResolvedPath`
/// contains some errors; it's always reported from [`PathAnalysisPass`].
pub fn resolve_path_early(
    db: &dyn HirAnalysisDb,
    path: PathId,
    scope: ScopeId,
) -> EarlyResolvedPath {
    resolve_segments_early(db, path.segments(db.as_hir_db()), scope)
}

/// Resolves the given path segments in the given scope.
/// It's not necessary to report any error even if the `EarlyResolvedPath`
/// contains some errors; it's always reported from [`PathAnalysisPass`].
pub fn resolve_segments_early(
    db: &dyn HirAnalysisDb,
    segments: &[Partial<IdentId>],
    scope: ScopeId,
) -> EarlyResolvedPath {
    // Obtain cache store for the given scope.
    let cache_store = resolve_path_early_impl(db, scope.top_mod(db.as_hir_db()));
    let importer = DefaultImporter;
    // We use the cache store that is returned from `resolve_path_early` to get
    // cached results immediately.
    let mut name_resolver = name_resolver::NameResolver::new_no_cache(db, &importer);

    let mut resolver = EarlyPathResolver::new(db, &mut name_resolver, cache_store);
    match resolver.resolve_segments(segments, scope) {
        Ok(res) => res.resolved,
        Err(_) => {
            // It's ok to ignore the errors here and returns an empty bucket because the
            // precise errors are reported from `PathAnalysisPass`.
            let bucket = NameResBucket::default();
            EarlyResolvedPath::Full(bucket)
        }
    }
}

/// Performs import resolution analysis. This pass only checks correctness of
/// the imports and doesn't emit other name resolutions errors.
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
        resolve_imports::accumulated::<ImportResolutionDiagAccumulator>(self.db, ingot)
            .into_iter()
            .filter_map(|diag| (diag.top_mod(self.db) == top_mod).then(|| Box::new(diag) as _))
            .collect()
    }
}

/// Performs path resolution analysis. This pass checks all paths appeared in a
/// module for
/// - Existence
/// - Visibility
/// - Domain correctness
/// - Ambiguity
///
/// NOTE: This pass doesn't check the conflict of item definitions or import
/// errors. If you need to check them, please consider using
/// [`ImportAnalysisPass`] or [`DefConflictAnalysisPass`].
pub struct PathAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> PathAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for PathAnalysisPass<'db> {
    fn run_on_module(&mut self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>> {
        let errors =
            resolve_path_early_impl::accumulated::<NameResolutionDiagAccumulator>(self.db, top_mod);

        errors
            .into_iter()
            .filter_map(|err| {
                (!matches!(err, NameResDiag::Conflict(..))).then(|| Box::new(err) as _)
            })
            .collect()
    }
}

/// Performs conflict analysis. This pass checks the conflict of item
/// definitions.
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
            resolve_path_early_impl::accumulated::<NameResolutionDiagAccumulator>(self.db, top_mod);

        // TODO: `ImplCollector`.
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
        ImportResolutionDiagAccumulator::push(db, diag);
    }

    imports
}

/// Performs early path resolution and cache the resolutions for paths appeared
/// in the given module. Also checks the conflict of the item definitions.
///
/// NOTE: This method doesn't check
/// - the conflict in impl/impl-trait blocks since it requires ingot granularity
///   analysis.
/// - the path resolution errors at expression and statement level since it
///   generally requires type analysis
#[salsa::tracked(return_ref)]
pub(crate) fn resolve_path_early_impl(
    db: &dyn HirAnalysisDb,
    top_mod: TopLevelMod,
) -> ResolvedQueryCacheStore {
    let importer = DefaultImporter;
    let mut visitor = EarlyPathVisitor::new(db, &importer);

    let mut ctxt = VisitorCtxt::with_item(db.as_hir_db(), top_mod.into());
    visitor.visit_item(&mut ctxt, top_mod.into());

    for diag in visitor.diags {
        NameResolutionDiagAccumulator::push(db, diag);
    }
    visitor.inner.into_cache_store()
}

struct EarlyPathVisitor<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    inner: name_resolver::NameResolver<'db, 'a>,
    diags: Vec<diagnostics::NameResDiag>,
    item_stack: Vec<ItemKind>,
    path_ctxt: Vec<ExpectedPathKind>,

    /// The set of scopes that have already been conflicted to avoid duplicate
    /// diagnostics.
    already_conflicted: FxHashSet<ScopeId>,
}

impl<'db, 'a> EarlyPathVisitor<'db, 'a> {
    fn new(db: &'db dyn HirAnalysisDb, importer: &'a DefaultImporter) -> Self {
        let resolver = name_resolver::NameResolver::new(db, importer);
        Self {
            db: db.as_hir_analysis_db(),
            inner: resolver,
            diags: Vec::new(),
            item_stack: Vec::new(),
            path_ctxt: Vec::new(),
            already_conflicted: FxHashSet::default(),
        }
    }

    fn verify_path(
        &mut self,
        path: PathId,
        scope: ScopeId,
        span: LazyPathSpan,
        bucket: NameResBucket,
    ) {
        let path_kind = self.path_ctxt.last().unwrap();
        let last_seg_idx = path.len(self.db.as_hir_db()) - 1;
        let last_seg_ident = *path.segments(self.db.as_hir_db())[last_seg_idx].unwrap();
        let span = span.segment(last_seg_idx).into();

        if bucket.is_empty() {
            let Err(err) = bucket.pick(path_kind.domain()) else {
                unreachable!()
            };

            match err {
                NameResolutionError::NotFound => {
                    self.diags
                        .push(NameResDiag::not_found(span, last_seg_ident));
                }
                NameResolutionError::Ambiguous(cands) => {
                    self.diags.push(NameResDiag::ambiguous(
                        self.db,
                        span,
                        last_seg_ident,
                        cands.clone(),
                    ));
                }
                _ => {}
            };

            return;
        }

        match path_kind.pick(bucket) {
            // The path exists and belongs to the expected kind.
            Either::Left(res) => {
                if !res.is_visible(self.db, scope) {
                    self.diags.push(NameResDiag::invisible(
                        span,
                        last_seg_ident,
                        res.derived_from(self.db),
                    ));
                }
            }

            // The path exists but doesn't belong to the expected kind.
            Either::Right(res) => match path_kind {
                ExpectedPathKind::Type => {
                    self.diags
                        .push(NameResDiag::ExpectedType(span, last_seg_ident, res));
                }

                ExpectedPathKind::Trait => {
                    self.diags
                        .push(NameResDiag::ExpectedTrait(span, last_seg_ident, res));
                }

                ExpectedPathKind::Value => {
                    self.diags
                        .push(NameResDiag::ExpectedValue(span, last_seg_ident, res));
                }
            },
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
        match binding.pick(domain) {
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

impl<'db, 'a> Visitor for EarlyPathVisitor<'db, 'a> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'_, LazyItemSpan>, item: ItemKind) {
        // We don't need to check use statements for conflicts because they are
        // already checked in import resolution.
        if matches!(item, ItemKind::Use(_)) {
            return;
        }

        let scope = ScopeId::from_item(item);
        // We don't need to check impl/impl-trait blocks for conflicts because they
        // needs ingot granularity analysis, the conflict checks for them is done by the
        // `ImplCollector`.
        if !matches!(item, ItemKind::Impl(_) | ItemKind::ImplTrait(_)) {
            self.check_conflict(scope);
        }

        self.item_stack.push(item);
        if matches!(item, ItemKind::Body(_)) {
            self.path_ctxt.push(ExpectedPathKind::Value);
        } else {
            self.path_ctxt.push(ExpectedPathKind::Type);
        }

        walk_item(self, ctxt, item);

        self.item_stack.pop();
        self.path_ctxt.pop();
    }

    fn visit_type_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTypeBoundSpan>,
        bound: &TypeBound,
    ) {
        self.path_ctxt.push(ExpectedPathKind::Trait);
        walk_type_bound(self, ctxt, bound);
        self.path_ctxt.pop();
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFieldDefSpan>,
        field: &hir::hir_def::FieldDef,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefSpan>,
        variant: &hir::hir_def::VariantDef,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyGenericParamSpan>,
        param: &hir::hir_def::GenericParam,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_generic_param(self, ctxt, param);
    }

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, ty: TypeId) {
        self.path_ctxt.push(ExpectedPathKind::Type);
        walk_ty(self, ctxt, ty);
        self.path_ctxt.pop();
    }

    // We don't need to run path analysis on patterns, statements and expressions in
    // early path resolution.
    fn visit_pat(&mut self, _: &mut VisitorCtxt<'_, LazyPatSpan>, _: PatId, _: &Pat) {}

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyExprSpan>,
        expr: ExprId,
        expr_data: &Expr,
    ) {
        if matches!(expr_data, Expr::Block(_)) {
            walk_expr(self, ctxt, expr)
        }
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'_, LazyPathSpan>, path: PathId) {
        let scope = ctxt.scope();
        let dummy_cache_store = ResolvedQueryCacheStore::no_cache();

        let mut resolver = EarlyPathResolver::new(self.db, &mut self.inner, &dummy_cache_store);
        let resolved_path = match resolver.resolve_path(path, scope) {
            Ok(bucket) => bucket,

            Err(err) => {
                let failed_at = err.failed_at;
                let span = ctxt.span().unwrap().segment(failed_at);
                let ident = path.segments(self.db.as_hir_db())[failed_at];

                let diag = match err.kind {
                    NameResolutionError::NotFound => {
                        NameResDiag::not_found(span.into(), *ident.unwrap())
                    }

                    NameResolutionError::Invalid => {
                        return;
                    }

                    NameResolutionError::Invisible(_) => {
                        unreachable!("`EarlyPathResolver doesn't check visibility");
                    }

                    NameResolutionError::Ambiguous(cands) => {
                        NameResDiag::ambiguous(self.db, span.into(), *ident.unwrap(), cands)
                    }

                    NameResolutionError::InvalidPathSegment(res) => {
                        NameResDiag::invalid_use_path_segment(
                            self.db,
                            span.into(),
                            *ident.unwrap(),
                            res,
                        )
                    }

                    NameResolutionError::Conflict(name, spans) => {
                        NameResDiag::Conflict(name, spans)
                    }
                };

                self.diags.push(diag);
                return;
            }
        };

        if let Some((idx, res)) = resolved_path.find_invisible_segment(self.db) {
            let span = ctxt.span().unwrap().segment(idx);
            let ident = path.segments(self.db.as_hir_db())[idx].unwrap();
            let diag = NameResDiag::invisible(span.into(), *ident, res.derived_from(self.db));
            self.diags.push(diag);
            return;
        }

        let EarlyResolvedPath::Full(bucket) = resolved_path.resolved else {
            return;
        };
        self.verify_path(path, scope, ctxt.span().unwrap(), bucket);
    }
}

#[derive(Debug, Clone, Copy)]
enum ExpectedPathKind {
    Type,
    Trait,
    Value,
}

impl ExpectedPathKind {
    fn domain(self) -> NameDomain {
        match self {
            ExpectedPathKind::Type => NameDomain::Type,
            ExpectedPathKind::Trait => NameDomain::Type,
            ExpectedPathKind::Value => NameDomain::Value,
        }
    }

    fn pick(self, bucket: NameResBucket) -> Either<NameRes, NameRes> {
        debug_assert!(!bucket.is_empty());

        let res = match bucket.pick(self.domain()).as_ref().ok() {
            Some(res) => res.clone(),
            None => {
                return Either::Right(bucket.into_iter().find_map(|res| res.ok()).unwrap());
            }
        };

        match self {
            Self::Type if !res.is_type() => Either::Right(res),
            Self::Trait if !res.is_trait() => Either::Right(res),
            Self::Value if !res.is_value() => Either::Right(res),
            _ => Either::Left(res),
        }
    }
}
