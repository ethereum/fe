pub mod diagnostics;

mod import_resolver;
mod name_resolver;
mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;

use hir::{
    hir_def::{
        scope_graph::{ScopeGraph, ScopeId},
        Expr, ExprId, GenericArgListId, IdentId, IngotId, ItemKind, Pat, PatId, PathId,
        TopLevelMod, TraitRefId, TypeId,
    },
    visitor::prelude::*,
};
pub use import_resolver::ResolvedImports;
pub use name_resolver::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind,
    NameResolutionError, QueryDirective,
};
use path_resolver::resolve_path_with_observer;
pub use path_resolver::{
    resolve_ident_to_bucket, resolve_name_res, resolve_path, PathRes, PathResError,
    PathResErrorKind, ResolvedVariant,
};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
pub use traits_in_scope::available_traits_in_scope;
pub(crate) use visibility_checker::is_scope_visible_from;

use self::{diagnostics::NameResDiag, import_resolver::DefaultImporter};
use crate::{analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher, HirAnalysisDb};

#[salsa::tracked(return_ref)]
pub fn resolve_query<'db>(
    db: &'db dyn HirAnalysisDb,
    query: EarlyNameQueryId<'db>,
) -> NameResBucket<'db> {
    let importer = DefaultImporter;
    let mut name_resolver = name_resolver::NameResolver::new(db, &importer);
    name_resolver.resolve_query(query)
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
        let ingot = top_mod.ingot(self.db);
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
        let mut ctxt = VisitorCtxt::with_item(self.db, top_mod.into());
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

impl<'db> ModuleAnalysisPass<'db> for DefConflictAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let graph = top_mod.scope_graph(self.db);

        walk(self.db, graph, top_mod.scope())
            .into_iter()
            .map(|d| Box::new(d) as _)
            .collect()
    }
}

pub struct DefConflictError<'db>(pub SmallVec<ItemKind<'db>, 2>);

fn walk<'db>(
    db: &'db dyn HirAnalysisDb,
    graph: &ScopeGraph<'db>,
    scope: ScopeId<'db>,
) -> Vec<DefConflictError<'db>> {
    let mut work: Vec<ScopeId<'db>> = vec![scope];

    #[derive(Hash, PartialEq, Eq)]
    enum Domain {
        Type,
        Val,
    }

    let mut defs = FxHashMap::<(Domain, IdentId<'db>), SmallVec<ItemKind<'db>, 2>>::default();
    let mut diags = vec![];

    while let Some(scope) = work.pop() {
        for item in graph.child_items(scope).filter(|i| i.name(db).is_some()) {
            let domain = match item {
                ItemKind::Func(_) | ItemKind::Const(_) => Domain::Val,

                ItemKind::Mod(_)
                | ItemKind::Struct(_)
                | ItemKind::Contract(_)
                | ItemKind::Enum(_)
                | ItemKind::TypeAlias(_)
                | ItemKind::Trait(_) => Domain::Type,

                ItemKind::TopMod(_)
                | ItemKind::Use(_)
                | ItemKind::Impl(_)
                | ItemKind::ImplTrait(_)
                | ItemKind::Body(_) => continue,
            };
            defs.entry((domain, item.name(db).unwrap()))
                .or_default()
                .push(item);
            if matches!(item, ItemKind::Mod(_)) {
                work.push(item.scope());
            }
        }
        diags.extend(
            defs.drain()
                .filter_map(|(_k, v)| (v.len() > 1).then_some(v))
                .map(DefConflictError),
        )
    }
    diags
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
            db,
            inner: resolver,
            diags: Vec::new(),
            item_stack: Vec::new(),
            path_ctxt: Vec::new(),
            already_conflicted: FxHashSet::default(),
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
                        conflicted_scope.name_span(self.db)
                    })
                    .collect();

                let diag = diagnostics::NameResDiag::Conflict(
                    scope.name(self.db).unwrap(),
                    conflicted_span,
                );
                self.diags.push(diag);
            }

            Err(_) => unreachable!(),
        };
    }

    fn make_query_for_conflict_check(&self, scope: ScopeId<'db>) -> Option<EarlyNameQueryId<'db>> {
        let name = scope.name(self.db)?;
        let directive = QueryDirective::new()
            .disallow_lex()
            .disallow_glob()
            .disallow_external();

        let parent_scope = scope.parent(self.db)?;
        Some(EarlyNameQueryId::new(
            self.db,
            name,
            parent_scope,
            directive,
        ))
    }
}

impl<'db> Visitor<'db> for EarlyPathVisitor<'db, '_> {
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

        let mut invisible = None;

        let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let expected_path_kind = *self.path_ctxt.last().unwrap();
        let resolve_tail_as_value = expected_path_kind.domain().contains(NameDomain::VALUE);

        let res = match resolve_path_with_observer(
            self.db,
            path,
            scope,
            resolve_tail_as_value,
            &mut check_visibility,
        ) {
            Ok(res) => res,

            Err(err) => {
                let failed_at = err.failed_at;
                let span = ctxt
                    .span()
                    .unwrap()
                    .segment(failed_at.segment_index(self.db))
                    .ident();

                let Some(ident) = failed_at.ident(self.db).to_opt() else {
                    return;
                };

                let diag = match err.kind {
                    PathResErrorKind::ParseError => unreachable!(),
                    PathResErrorKind::NotFound(bucket) => {
                        if path.len(self.db) == 1
                            && matches!(
                                self.path_ctxt.last().unwrap(),
                                ExpectedPathKind::Expr | ExpectedPathKind::Pat
                            )
                        {
                            return;
                        } else if let Some(nr) = bucket.iter_ok().next() {
                            if path != err.failed_at {
                                NameResDiag::InvalidPathSegment(
                                    span.into(),
                                    ident,
                                    nr.kind.name_span(self.db),
                                )
                            } else {
                                match expected_path_kind {
                                    ExpectedPathKind::Record | ExpectedPathKind::Type => {
                                        NameResDiag::ExpectedType(
                                            span.into(),
                                            ident,
                                            nr.kind_name(),
                                        )
                                    }
                                    ExpectedPathKind::Trait => NameResDiag::ExpectedTrait(
                                        span.into(),
                                        ident,
                                        nr.kind_name(),
                                    ),
                                    ExpectedPathKind::Value => NameResDiag::ExpectedValue(
                                        span.into(),
                                        ident,
                                        nr.kind_name(),
                                    ),
                                    _ => NameResDiag::NotFound(span.into(), ident),
                                }
                            }
                        } else {
                            NameResDiag::NotFound(span.into(), ident)
                        }
                    }

                    PathResErrorKind::Ambiguous(cands) => {
                        NameResDiag::ambiguous(self.db, span.into(), ident, cands)
                    }

                    PathResErrorKind::AssocTy(_) => todo!(),
                    PathResErrorKind::TraitMethodNotFound(_) => todo!(),
                    PathResErrorKind::TooManyGenericArgs { expected, given } => {
                        NameResDiag::TooManyGenericArgs {
                            span: span.into(),
                            expected,
                            given,
                        }
                    }

                    PathResErrorKind::InvalidPathSegment(res) => {
                        // res.name_span(db)
                        NameResDiag::InvalidPathSegment(span.into(), ident, res.name_span(self.db))
                    }

                    PathResErrorKind::Conflict(spans) => NameResDiag::Conflict(ident, spans),
                };

                self.diags.push(diag);
                return;
            }
        };

        if let Some((path, deriv_span)) = invisible {
            let span = ctxt
                .span()
                .unwrap()
                .segment(path.segment_index(self.db))
                .ident();

            let ident = path.ident(self.db);
            let diag = NameResDiag::Invisible(span.into(), *ident.unwrap(), deriv_span);
            self.diags.push(diag);
        }

        let is_type = matches!(res, PathRes::Ty(_) | PathRes::TyAlias(..));
        let is_trait = matches!(res, PathRes::Trait(_));

        let span = ctxt
            .span()
            .unwrap()
            .segment(path.segment_index(self.db))
            .into();

        let ident = path.ident(self.db).to_opt().unwrap();

        match expected_path_kind {
            ExpectedPathKind::Type if !is_type => {
                self.diags
                    .push(NameResDiag::ExpectedType(span, ident, res.kind_name()))
            }

            ExpectedPathKind::Trait if !is_trait => {
                self.diags
                    .push(NameResDiag::ExpectedTrait(span, ident, res.kind_name()))
            }

            ExpectedPathKind::Value if is_type || is_trait => self
                .diags
                .push(NameResDiag::ExpectedValue(span, ident, res.kind_name())),

            _ => {}
        }

        walk_path(self, ctxt, path);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
}
