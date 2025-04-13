pub mod diagnostics;

mod import_resolver;
mod name_resolver;
mod path_resolver;
pub(crate) mod traits_in_scope;
mod visibility_checker;

use hir::{
    hir_def::{
        Expr, ExprId, GenericArgListId, IngotId, ItemKind, Pat, PatId, PathId, TopLevelMod,
        TraitRefId, TypeId,
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
pub struct ImportAnalysisPass {}

impl ModuleAnalysisPass for ImportAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let ingot = top_mod.ingot(db);
        resolve_imports(db, ingot)
            .0
            .iter()
            .filter(|diag| diag.top_mod(db) == top_mod)
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
pub struct PathAnalysisPass {}

/// TODO: Remove this!!!!
impl ModuleAnalysisPass for PathAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut visitor = EarlyPathVisitor::new(db);
        let mut ctxt = VisitorCtxt::with_item(db, top_mod.into());
        visitor.visit_item(&mut ctxt, top_mod.into());

        visitor
            .diags
            .into_iter()
            .map(|diag| Box::new(diag) as _)
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

struct EarlyPathVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<diagnostics::NameResDiag<'db>>,
    item_stack: Vec<ItemKind<'db>>,
    path_ctxt: Vec<ExpectedPathKind>,
}

impl<'db> EarlyPathVisitor<'db> {
    fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            diags: Vec::new(),
            item_stack: Vec::new(),
            path_ctxt: Vec::new(),
        }
    }
}

impl<'db> Visitor<'db> for EarlyPathVisitor<'db> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>, item: ItemKind<'db>) {
        // We don't need to check use statements for conflicts because they are
        // already checked in import resolution.
        if matches!(item, ItemKind::Use(_)) {
            return;
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
                        // res.name_span(self.db)
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
