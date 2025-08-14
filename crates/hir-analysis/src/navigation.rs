use hir::{
    hir_def::{scope_graph::ScopeId, Body, Expr, ExprId, ItemKind, Partial, Pat, PatId, Stmt, StmtId, PathId},
    synthesis::{HirNodeContext, LazyHirResult},
    SpannedHirDb,
};
use crate::HirAnalysisDb;
use hir::visitor::Visitor;
use crate::ty::method_table::probe_method_uncanonicalized;

use crate::ty::ty_check::{check_func_body, RecordLike};
use crate::name_resolution::{
    local_binding_span_for_expr, resolve_ident_to_bucket, resolve_path, resolve_path_segment,
    NameDomain, NameResKind, PathResErrorKind,
};

#[derive(Debug)]
pub enum NavTarget<'db> {
    Scope(ScopeId<'db>),
    Span(common::diagnostics::Span),
}

pub fn resolve_goto<'db>(
    db: &'db dyn HirAnalysisDb,
    span_db: &'db dyn SpannedHirDb,
    result: LazyHirResult<'db>,
) -> Vec<NavTarget<'db>> {
    match result {
        LazyHirResult::Expr(body, expr_id, context) =>
            resolve_expression_definition(db, span_db, body, expr_id, context),
        LazyHirResult::Stmt(body, stmt_id, context) =>
            resolve_statement_definition(db, span_db, body, stmt_id, context),
        LazyHirResult::Pat(body, pat_id) => resolve_pattern_definition(db, span_db, body, pat_id),
        LazyHirResult::ItemPath(item, path, _ctx, seg) => {
            let scope = ScopeId::from_item(item);
            if let Some(segment_index) = seg {
                match resolve_path_segment(db, path, segment_index, scope, true) {
                    Ok(res) => {
                        // special-case enum variant targeting
                        if let crate::name_resolution::PathRes::EnumVariant(v) = &res {
                            vec![NavTarget::Scope(ScopeId::Variant(v.variant))]
                        } else {
                            res.as_scope(db).map(NavTarget::Scope).into_iter().collect()
                        }
                    }
                    Err(err) => match err.kind {
                        PathResErrorKind::NotFound(bucket) => bucket
                            .iter_ok()
                            .flat_map(|r| r.scope())
                            .map(NavTarget::Scope)
                            .collect(),
                        PathResErrorKind::Ambiguous(vec) =>
                            vec.into_iter().flat_map(|r| r.scope()).map(NavTarget::Scope).collect(),
                        _ => vec![],
                    },
                }
            } else {
                match resolve_path(db, path, scope, false) {
                    Ok(res) => {
                        if let crate::name_resolution::PathRes::EnumVariant(v) = &res {
                            vec![NavTarget::Scope(ScopeId::Variant(v.variant))]
                        } else {
                            res.as_scope(db).map(NavTarget::Scope).into_iter().collect()
                        }
                    }
                    Err(err) => match err.kind {
                        PathResErrorKind::NotFound(bucket) => bucket
                            .iter_ok()
                            .flat_map(|r| r.scope())
                            .map(NavTarget::Scope)
                            .collect(),
                        PathResErrorKind::Ambiguous(vec) =>
                            vec.into_iter().flat_map(|r| r.scope()).map(NavTarget::Scope).collect(),
                        _ => vec![],
                    },
                }
            }
        }
        LazyHirResult::ItemGenericParam(item, idx) => vec![NavTarget::Scope(ScopeId::GenericParam(item, idx))],
        LazyHirResult::ItemType(_, _, _) | LazyHirResult::None => vec![],
    }
}

fn resolve_expression_definition<'db>(
    db: &'db dyn HirAnalysisDb,
    span_db: &'db dyn SpannedHirDb,
    body: Body<'db>,
    expr_id: ExprId,
    context: HirNodeContext,
) -> Vec<NavTarget<'db>> {
    let expr_data = &body.exprs(span_db)[expr_id];

    // Method call: resolve method on receiver type
    if let HirNodeContext::MethodCall = context {
        if let Partial::Present(Expr::MethodCall(receiver_expr_id, method_name, _gen_args, _call_args)) = expr_data {
            if let Some(func) = find_containing_func(db, body) {
                let (_, typed_body) = crate::ty::ty_check::check_func_body(db, func);
                let recv_ty = typed_body.expr_ty(db, *receiver_expr_id);
                if let Some(name_ident) = method_name.to_opt() {
                    let ingot = body.top_mod(db).ingot(db);
                    let cands = probe_method_uncanonicalized(db, ingot, recv_ty, name_ident);
                    let scopes = cands.into_iter().map(|fd| NavTarget::Scope(fd.scope(db))).collect::<Vec<_>>();
                    if !scopes.is_empty() { return scopes; }
                }
            }
        }
        return vec![];
    }

    if let HirNodeContext::FieldAccess = context {
        if let Partial::Present(Expr::Field(receiver_expr_id, field_index)) = expr_data {
            if let Some(func) = find_containing_func(db, body) {
                let (_, typed_body) = check_func_body(db, func);
                let receiver_ty = typed_body.expr_ty(db, *receiver_expr_id);
                if let Some(field_idx) = field_index.to_opt() {
                    if let hir::hir_def::FieldIndex::Ident(ident) = field_idx {
                        let record_like = RecordLike::from_ty(receiver_ty);
                        if let Some(scope) = record_like.record_field_scope(db, ident) {
                            return vec![NavTarget::Scope(scope)];
                        }
                    }
                }
            }
        }
        return vec![];
    }

    match expr_data {
        Partial::Present(Expr::Path(Partial::Present(path_id))) => {
            let scope = body.scope();

            if path_id.is_bare_ident(db) {
                if let Ok(path_res) = resolve_path_segment(db, *path_id, 0, body.scope(), true) {
                    if let crate::name_resolution::PathRes::FuncParam(..) = path_res {
                        if let Some(s) = path_res.as_scope(db) {
                            return vec![NavTarget::Scope(s)];
                        }
                    }
                }
                if let Some(func) = find_containing_func(db, body) {
                    if let Some(span) = local_binding_span_for_expr(db, span_db, func, expr_id) {
                        return vec![NavTarget::Span(span)];
                    }
                }
            }

            match resolve_path(db, *path_id, scope, true) {
                Ok(path_res) => {
                    if let crate::name_resolution::PathRes::EnumVariant(v) = &path_res {
                        vec![NavTarget::Scope(ScopeId::Variant(v.variant))]
                    } else {
                        path_res.as_scope(db).map(NavTarget::Scope).into_iter().collect()
                    }
                }
                Err(err) => match err.kind {
                    PathResErrorKind::NotFound(bucket) => bucket
                        .iter_ok()
                        .flat_map(|r| r.scope())
                        .map(NavTarget::Scope)
                        .collect(),
                    PathResErrorKind::Ambiguous(vec) =>
                        vec.into_iter().flat_map(|r| r.scope()).map(NavTarget::Scope).collect(),
                    _ => vec![],
                },
            }
        }
        _ => vec![],
    }
}

// Full helper that can fallback using cursor/top_mod when initial result is None or empty
pub fn resolve_goto_full<'db>(
    db: &'db dyn HirAnalysisDb,
    span_db: &'db dyn SpannedHirDb,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    cursor: parser::TextSize,
    result: LazyHirResult<'db>,
) -> Vec<NavTarget<'db>> {
    let out = resolve_goto(db, span_db, result.clone());
    if !out.is_empty() { return out; }
    // Fallback: find nearest path segment within enclosing item and resolve
    if let Some(item) = find_enclosing_item(span_db, top_mod, cursor) {
        let mut vctxt = hir::visitor::VisitorCtxt::with_item(span_db, item);
        let mut collector = PathSpanCollector::default();
        collector.visit_item(&mut vctxt, item);
        if let Some((seg_path, scope)) = find_path_segment_at_cursor(span_db, cursor, collector.paths) {
            match resolve_path_segment(db, seg_path, seg_path.segment_index(db), scope, true) {
                Ok(res) => {
                    if let crate::name_resolution::PathRes::EnumVariant(v) = &res {
                        return vec![NavTarget::Scope(ScopeId::Variant(v.variant))];
                    }
                    if let Some(s) = res.as_scope(db) {
                        return vec![NavTarget::Scope(s)];
                    }
                }
                Err(err) => match err.kind {
                    PathResErrorKind::NotFound(bucket) => {
                        let scopes = bucket.iter_ok().flat_map(|r| r.scope()).collect::<Vec<_>>();
                        if !scopes.is_empty() { return scopes.into_iter().map(NavTarget::Scope).collect(); }
                    }
                    PathResErrorKind::Ambiguous(vec) => {
                        let scopes = vec.into_iter().flat_map(|r| r.scope()).collect::<Vec<_>>();
                        if !scopes.is_empty() { return scopes.into_iter().map(NavTarget::Scope).collect(); }
                    }
                    _ => {}
                },
            }
        }
    }
    vec![]
}

fn find_enclosing_item<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    cursor: parser::TextSize,
) -> Option<ItemKind<'db>> {
    use hir::span::{DynLazySpan, LazySpan};
    let items = top_mod.scope_graph(db).items_dfs(db);
    let mut best = None;
    let mut size = None;
    for item in items {
        let span = DynLazySpan::from(item.span()).resolve(db).unwrap();
        if span.range.contains(cursor) {
            let len = span.range.end() - span.range.start();
            if size.map(|s| s > len).unwrap_or(true) { best = Some(item); size = Some(len); }
        }
    }
    best
}

#[derive(Default)]
struct PathSpanCollector<'db> {
    paths: Vec<(PathId<'db>, ScopeId<'db>, hir::span::path::LazyPathSpan<'db>)>,
}

impl<'db, 'ast: 'db> hir::visitor::Visitor<'ast> for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut hir::visitor::VisitorCtxt<'ast, hir::span::path::LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else { return; };
        let scope = ctxt.scope();
        self.paths.push((path, scope, span));
    }
}

fn find_path_segment_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    cursor: parser::TextSize,
    full_paths: Vec<(PathId<'db>, ScopeId<'db>, hir::span::path::LazyPathSpan<'db>)>,
) -> Option<(PathId<'db>, ScopeId<'db>)> {
    use hir::span::LazySpan;
    let mut best: Option<(PathId<'db>, ScopeId<'db>, hir::span::path::LazyPathSpan<'db>, parser::TextRange)> = None;
    for (path, scope, lazy_span) in full_paths {
        let span = lazy_span.resolve(db).unwrap();
        if span.range.contains(cursor) {
            let len = span.range.end() - span.range.start();
            if best.as_ref().map(|(_, _, _, r)| (r.end() - r.start()) > len).unwrap_or(true) {
                best = Some((path, scope, lazy_span, span.range));
            }
        }
    }
    let Some((path, scope, lazy_span, _)) = best.clone() else { return None };
    for idx in 0..=path.segment_index(db) {
        let seg_lazy = lazy_span.clone().segment(idx);
        let seg_full = seg_lazy.resolve(db).unwrap();
        if seg_full.range.contains(cursor) {
            if let Some(id_span) = seg_lazy.ident().resolve(db) {
                if id_span.range.contains(cursor) {
                    return Some((path.segment(db, idx).unwrap(), scope));
                }
            }
        }
    }
    None
}

fn resolve_statement_definition<'db>(
    db: &'db dyn HirAnalysisDb,
    span_db: &'db dyn SpannedHirDb,
    body: Body<'db>,
    stmt_id: StmtId,
    context: HirNodeContext,
) -> Vec<NavTarget<'db>> {
    let stmt_data = &body.stmts(span_db)[stmt_id];
    let mut out = vec![];
    if let HirNodeContext::PathSegment(segment_index) = context {
        if let Partial::Present(Stmt::Let(_pat_id, _ty, _init)) = stmt_data {
            // If the type annotation has a path, resolve by segment
            // Fall back to full path resolution if needed
        }
    }
    out
}

fn resolve_pattern_definition<'db>(
    _db: &'db dyn HirAnalysisDb,
    _span_db: &'db dyn SpannedHirDb,
    _body: Body<'db>,
    _pat_id: PatId,
) -> Vec<NavTarget<'db>> {
    vec![]
}

fn find_containing_func<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<hir::hir_def::Func<'db>> {
    use hir::hir_def::{BodyKind, ItemKind};
    if body.body_kind(db) != BodyKind::FuncBody {
        return None;
    }
    let top_mod = body.top_mod(db);
    let items = top_mod.scope_graph(db).items_dfs(db);
    for item in items {
        if let ItemKind::Func(func) = item {
            if func.body(db) == Some(body) {
                return Some(func);
            }
        }
    }
    None
}
