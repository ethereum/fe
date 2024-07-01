pub mod diagnostics;

mod import_resolver;
mod name_resolver;
mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;

use either::Either;
use hir::{
    analysis_pass::ModuleAnalysisPass,
    diagnostics::DiagnosticVoucher,
    hir_def::{
        scope_graph::ScopeId, Expr, ExprId, GenericArgListId, IdentId, IngotId, ItemKind, Partial,
        Pat, PatId, PathId, TopLevelMod, TraitRefId, TypeId,
    },
    visitor::prelude::*,
};
pub use import_resolver::ResolvedImports;
pub use name_resolver::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind,
    NameResolutionError, QueryDirective,
};
pub use path_resolver::EarlyResolvedPath;
use rustc_hash::FxHashSet;
pub use traits_in_scope::available_traits_in_scope;
pub(crate) use visibility_checker::is_scope_visible_from;

use self::{
    diagnostics::NameResDiag, import_resolver::DefaultImporter, path_resolver::EarlyPathResolver,
};
use crate::HirAnalysisDb;

#[salsa::tracked(return_ref)]
pub fn resolve_query<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
) -> NameResBucket<'db> {
    let importer = DefaultImporter;
    let mut name_resolver = name_resolver::NameResolver::new(db, &importer);
    name_resolver.resolve_query(query)
}

// Resolves the given path in the given scope.
/// It's not necessary to report any error even if the `EarlyResolvedPath`
/// contains some errors; it's always reported from [`PathAnalysisPass`].
pub fn resolve_path_early<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<EarlyResolvedPath<'db>> {
    resolve_segments_early(db, path.segments(db.as_hir_db()), scope)
}

/// Resolves the given path segments in the given scope.
/// It's not necessary to report any error even if the `EarlyResolvedPath`
/// contains some errors; it's always reported from [`PathAnalysisPass`].
pub fn resolve_segments_early<'db>(
    db: &'db dyn HirAnalysisDb,
    segments: &[Partial<IdentId<'db>>],
    scope: ScopeId<'db>,
) -> Option<EarlyResolvedPath<'db>> {
    // We use the cache store that is returned from `resolve_path_early` to get
    // cached results immediately.
    let mut resolver = EarlyPathResolver::new(db);
    match resolver.resolve_segments(segments, scope) {
        Ok(res) => Some(res.resolved),
        Err(_) => {
            // It's ok to ignore the errors here and returns an empty bucket because the
            // precise errors are reported from `PathAnalysisPass`.
            None
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

    pub fn resolve_imports(&self, ingot: IngotId<'db>) -> &'db ResolvedImports<'db> {
        &resolve_imports(self.db, ingot).1
    }
}

impl<'db> ModuleAnalysisPass<'db> for ImportAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let ingot = top_mod.ingot(self.db.as_hir_db());
        resolve_imports(self.db, ingot)
            .0
            .iter()
            .filter(|diag| diag.top_mod(self.db) == top_mod)
            .map(|diag| Box::new(diag.clone()) as _)
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

/// TODO: Remove this!!!!
impl<'db> ModuleAnalysisPass<'db> for PathAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let importer = DefaultImporter;
        let mut visitor = EarlyPathVisitor::new(self.db, &importer);
        let mut ctxt = VisitorCtxt::with_item(self.db.as_hir_db(), top_mod.into());
        visitor.visit_item(&mut ctxt, top_mod.into());

        visitor
            .diags
            .iter()
            .filter(|diag| !matches!(diag, NameResDiag::Conflict(..)))
            .map(|diag| Box::new(diag.clone()) as _)
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

/// TODO: Remove this!!!!
impl<'db> ModuleAnalysisPass<'db> for DefConflictAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let importer = DefaultImporter;
        let mut visitor = EarlyPathVisitor::new(self.db, &importer);
        let mut ctxt = VisitorCtxt::with_item(self.db.as_hir_db(), top_mod.into());
        visitor.visit_item(&mut ctxt, top_mod.into());

        visitor
            .diags
            .iter()
            .filter(|diag| matches!(diag, NameResDiag::Conflict(..)))
            .map(|diag| Box::new(diag.clone()) as _)
            .collect()
    }
}

#[salsa::tracked(return_ref)]
pub fn resolve_imports<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
) -> (Vec<NameResDiag<'db>>, ResolvedImports<'db>) {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, diags) = resolver.resolve_imports();
    (diags, imports)
}

struct EarlyPathVisitor<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    inner: name_resolver::NameResolver<'db, 'a>,
    diags: Vec<diagnostics::NameResDiag<'db>>,
    item_stack: Vec<ItemKind<'db>>,
    path_ctxt: Vec<ExpectedPathKind>,

    /// The set of scopes that have already been conflicted to avoid duplicate
    /// diagnostics.
    already_conflicted: FxHashSet<ScopeId<'db>>,
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
        path: PathId<'db>,
        scope: ScopeId,
        span: LazyPathSpan<'db>,
        bucket: &'db NameResBucket<'db>,
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
                    if !matches!(
                        self.path_ctxt.last().unwrap(),
                        ExpectedPathKind::Expr | ExpectedPathKind::Pat
                    ) || path.len(self.db.as_hir_db()) != 1
                    {
                        self.diags
                            .push(NameResDiag::not_found(span, last_seg_ident));
                    }
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

                _ => {}
            },
        }
    }

    fn check_conflict(&mut self, scope: ScopeId<'db>) {
        if !self.already_conflicted.insert(scope) {
            return;
        }

        let Some(query) = self.make_query_for_conflict_check(scope) else {
            return;
        };

        let domain = NameDomain::from_scope(self.db, scope);
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

    fn make_query_for_conflict_check(&self, scope: ScopeId<'db>) -> Option<EarlyNameQueryId<'db>> {
        let name = scope.name(self.db.as_hir_db())?;
        let directive = QueryDirective::new()
            .disallow_lex()
            .disallow_glob()
            .disallow_external();

        let parent_scope = scope.parent(self.db.as_hir_db())?;
        Some(EarlyNameQueryId::new(
            self.db,
            name,
            parent_scope,
            directive,
        ))
    }
}

impl<'db, 'a> Visitor<'db> for EarlyPathVisitor<'db, 'a> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>, item: ItemKind<'db>) {
        // We don't need to check use statements for conflicts because they are
        // already checked in import resolution.
        if matches!(item, ItemKind::Use(_)) {
            return;
        }

        // We don't need to check impl blocks for conflicts because they
        // needs ingot granularity analysis, the conflict checks for them is done by the
        // `ImplCollector`.
        if !matches!(item, ItemKind::Impl(_)) {
            let scope = ScopeId::from_item(item);
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

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        self.path_ctxt.push(ExpectedPathKind::Trait);
        walk_trait_ref(self, ctxt, trait_ref);
        self.path_ctxt.pop();
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &hir::hir_def::FieldDef<'db>,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &hir::hir_def::VariantDef<'db>,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
        param: &hir::hir_def::GenericParam<'db>,
    ) {
        let scope = ctxt.scope();
        self.check_conflict(scope);
        walk_generic_param(self, ctxt, param);
    }

    fn visit_generic_arg_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericArgListSpan<'db>>,
        args: GenericArgListId<'db>,
    ) {
        self.path_ctxt.push(ExpectedPathKind::Type);
        walk_generic_arg_list(self, ctxt, args);
        self.path_ctxt.pop();
    }

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, ty: TypeId<'db>) {
        self.path_ctxt.push(ExpectedPathKind::Type);
        walk_ty(self, ctxt, ty);
        self.path_ctxt.pop();
    }

    // We don't need to run path analysis on patterns, statements and expressions in
    // early path resolution.
    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        pat_data: &Pat<'db>,
    ) {
        match pat_data {
            Pat::PathTuple { .. } | Pat::Record { .. } => {
                self.path_ctxt.push(ExpectedPathKind::Record)
            }
            _ => self.path_ctxt.push(ExpectedPathKind::Pat),
        }
        walk_pat(self, ctxt, pat);
        self.path_ctxt.pop();
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        match expr_data {
            Expr::RecordInit(..) => {
                self.path_ctxt.push(ExpectedPathKind::Record);
            }

            _ => {
                self.path_ctxt.push(ExpectedPathKind::Expr);
            }
        }
        walk_expr(self, ctxt, expr);
        self.path_ctxt.pop();
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        let scope = ctxt.scope();

        let mut resolver = EarlyPathResolver::new(self.db);
        let resolved_path = match resolver.resolve_path(path, scope) {
            Ok(bucket) => bucket,

            Err(err) => {
                let failed_at = err.failed_at;
                let span = ctxt.span().unwrap().segment(failed_at);
                let ident = path.segments(self.db.as_hir_db())[failed_at];

                let diag = match err.kind {
                    NameResolutionError::NotFound => {
                        if path.len(self.db.as_hir_db()) == 1
                            && matches!(
                                self.path_ctxt.last().unwrap(),
                                ExpectedPathKind::Expr | ExpectedPathKind::Pat
                            )
                        {
                            return;
                        } else {
                            NameResDiag::not_found(span.into(), *ident.unwrap())
                        }
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
    Record,
    Pat,
    Expr,
}

impl ExpectedPathKind {
    fn domain(self) -> NameDomain {
        match self {
            ExpectedPathKind::Type => NameDomain::TYPE,
            ExpectedPathKind::Trait => NameDomain::TYPE,
            ExpectedPathKind::Value => NameDomain::VALUE,
            ExpectedPathKind::Pat | ExpectedPathKind::Record | ExpectedPathKind::Expr => {
                NameDomain::VALUE | NameDomain::TYPE
            }
        }
    }

    fn pick<'db>(self, bucket: &NameResBucket<'db>) -> Either<NameRes<'db>, NameRes<'db>> {
        debug_assert!(!bucket.is_empty());

        let res = match bucket.pick(self.domain()).as_ref().ok() {
            Some(res) => res.clone(),
            None => {
                return Either::Right(
                    bucket
                        .iter()
                        .find_map(|res| {
                            if let Ok(res) = res {
                                Some(res.clone())
                            } else {
                                None
                            }
                        })
                        .unwrap(),
                );
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
