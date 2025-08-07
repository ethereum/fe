use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind, PathId, TopLevelMod},
    lower::map_file_to_mod,
    span::{DynLazySpan, LazySpan},
    visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt},
    SpannedHirDb,
};
use hir_analysis::name_resolution::{resolve_path, PathResErrorKind};
use tracing::{debug, error};

use crate::{backend::Backend, util::to_offset_from_position};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

#[derive(Default)]
#[allow(dead_code)]
struct PathSpanCollector<'db> {
    paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else {
            return;
        };

        let scope = ctxt.scope();
        self.paths.push((path, scope, span));
    }
}

#[allow(dead_code)]
fn find_path_surrounding_cursor<'db>(
    db: &'db DriverDataBase,
    cursor: Cursor,
    full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
) -> Option<(PathId<'db>, bool, ScopeId<'db>)> {
    for (path, scope, lazy_span) in full_paths {
        let span = lazy_span.resolve(db).unwrap();
        if span.range.contains(cursor) {
            for idx in 0..=path.segment_index(db) {
                let seg_span = lazy_span.clone().segment(idx).resolve(db).unwrap();
                if seg_span.range.contains(cursor) {
                    return Some((
                        path.segment(db, idx).unwrap(),
                        idx != path.segment_index(db),
                        scope,
                    ));
                }
            }
        }
    }
    None
}

pub fn find_enclosing_item<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<ItemKind<'db>> {
    let items = top_mod.scope_graph(db).items_dfs(db);

    let mut smallest_enclosing_item = None;
    let mut smallest_range_size = None;

    for item in items {
        let lazy_item_span = DynLazySpan::from(item.span());
        let item_span = lazy_item_span.resolve(db).unwrap();

        if item_span.range.contains(cursor) {
            let range_size = item_span.range.end() - item_span.range.start();
            if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                smallest_enclosing_item = Some(item);
                smallest_range_size = Some(range_size);
            }
        }
    }

    smallest_enclosing_item
}

pub fn get_goto_target_scopes_for_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    let item: ItemKind = find_enclosing_item(db, top_mod, cursor)?;

    let mut visitor_ctxt = VisitorCtxt::with_item(db, item);
    let mut path_segment_collector = PathSpanCollector::default();
    path_segment_collector.visit_item(&mut visitor_ctxt, item);

    let (path, _is_intermediate, scope) =
        find_path_surrounding_cursor(db, cursor, path_segment_collector.paths)?;

    let resolved = resolve_path(db, path, scope, false);
    let scopes = match resolved {
        Ok(r) => r.as_scope(db).into_iter().collect::<Vec<_>>(),
        Err(err) => match err.kind {
            PathResErrorKind::NotFound(bucket) => {
                bucket.iter_ok().flat_map(|r| r.scope()).collect()
            }
            PathResErrorKind::Ambiguous(vec) => vec.into_iter().flat_map(|r| r.scope()).collect(),
            _ => vec![],
        },
    };

    Some(scopes)
}

/// Goto definition using HIR synthesis architecture integrated with semantic resolution
pub fn goto_definition_with_hir_synthesis<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Result<Vec<ScopeId<'db>>, Box<dyn std::error::Error>> {
    use crate::hir_integration::{lazy_hir_for_cursor, LazyHirResult};

    // Use the HIR synthesis system to find the HIR node at the cursor position
    let hir_result = lazy_hir_for_cursor(db, top_mod, cursor);

    debug!(
        "Cursor {:?}: HIR result = {:?}",
        cursor,
        match &hir_result {
            LazyHirResult::Expr(_, _, _) => "Expr",
            LazyHirResult::Stmt(_, _, _) => "Stmt",
            LazyHirResult::Pat(_, _) => "Pat",
            LazyHirResult::None => "None",
        }
    );

    // Debug: Log what HIR synthesis found
    match &hir_result {
        LazyHirResult::Expr(body, expr_id, context) => {
            debug!(
                "HIR synthesis found expression: body={:?}, expr_id={:?}, context={:?}",
                body, expr_id, context
            );
        }
        LazyHirResult::Stmt(body, stmt_id, context) => {
            debug!(
                "HIR synthesis found statement: body={:?}, stmt_id={:?}, context={:?}",
                body, stmt_id, context
            );
        }
        LazyHirResult::Pat(body, pat_id) => {
            debug!(
                "HIR synthesis found pattern: body={:?}, pat_id={:?}",
                body, pat_id
            );
        }
        LazyHirResult::None => {
            debug!(
                "HIR synthesis found nothing at cursor position {:?}",
                cursor
            );
        }
    }

    // Now resolve the HIR node to its semantic definition
    let result = match hir_result {
        LazyHirResult::Expr(body, expr_id, context) => {
            resolve_expression_definition(db, body, expr_id, context)
        }
        LazyHirResult::Stmt(body, stmt_id, context) => {
            resolve_statement_definition(db, body, stmt_id, context)
        }
        LazyHirResult::Pat(body, pat_id) => resolve_pattern_definition(db, body, pat_id),
        LazyHirResult::None => {
            // No HIR node found at this position - nothing to go to
            Ok(vec![])
        }
    };

    result
}

/// Resolve definition location for an expression HIR node
fn resolve_expression_definition<'db>(
    db: &'db DriverDataBase,
    body: hir::hir_def::Body<'db>,
    expr_id: hir::hir_def::ExprId,
    context: hir::synthesis::HirNodeContext,
) -> Result<Vec<ScopeId<'db>>, Box<dyn std::error::Error>> {
    use hir::hir_def::{Expr, Partial, PatId};
    use hir_analysis::ty::ty_check::env::{LocalBinding, TyCheckEnv};
    use hir_analysis::ty::ty_check::path::ResolvedPathInBody;

    // Get the actual expression data from its ID
    let expr_data = &body.exprs(db)[expr_id];

    // Special handling if we're on a field access
    if let hir::synthesis::HirNodeContext::FieldAccess = context {
        // This means the cursor is on the field name in a field access expression
        if let Partial::Present(Expr::Field(_receiver_expr_id, field_index)) = expr_data {
            if let Some(_field_idx) = field_index.to_opt() {
                // TODO: Implement proper field resolution
                // This requires accessing type information and resolving to field definitions
                // which involves private APIs in hir-analysis
                // For now, field goto is not fully implemented
            }
        }
        return Ok(vec![]);
    }

    // Match on the kind of expression to handle different semantic cases
    match expr_data {
        // This is the most common case: a variable, function call, type name, etc.
        Partial::Present(Expr::Path(Partial::Present(path_id))) => {
            let scope = body.scope();

            // First check if this is a bare identifier that might be a local variable
            if path_id.is_bare_ident(db) {
                // Try to resolve as a local binding using type checker's resolution
                let ident = *path_id.ident(db).unwrap();

                // We need to create a type check environment to look up local bindings
                // This is a simplified version - in reality we'd need the actual TyCheckEnv
                // from when the body was type-checked
                if let Some(typed_body) = db.typed_body(body.into()) {
                    // Check if this identifier resolves to a local binding
                    // We need to iterate through the body's patterns to find the binding
                    for (pat_id, pat) in body.pats(db).iter() {
                        if let Partial::Present(hir::hir_def::Pat::Bind(bind_name, _)) = pat {
                            if let Partial::Present(name) = bind_name {
                                if *name == ident {
                                    // Found the binding! Now get its source location
                                    if let Some(source_map) = db.body_source_map(body.into()) {
                                        if let Some(origin) =
                                            source_map.pat_map(db).node_to_source.get(pat_id)
                                        {
                                            // Convert the origin to a file location
                                            if let Some(span) =
                                                origin.syntax_node_ptr().map(|ptr| ptr.text_range())
                                            {
                                                // This is a local binding - return its definition location
                                                // We need to construct a proper ScopeId or return empty for now
                                                // since local bindings don't have ScopeIds
                                                return Ok(vec![]);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Check if we have segment context
            match context {
                hir::synthesis::HirNodeContext::PathSegment(segment_index) => {
                    // Resolve just the specific segment
                    if let Some(segment_path_id) = path_id.segment(db, segment_index) {
                        match resolve_path(db, segment_path_id, scope, true) {
                            Ok(path_res) => {
                                if let Some(scope) = path_res.as_scope(db) {
                                    return Ok(vec![scope]);
                                }
                            }
                            Err(_) => {}
                        }
                    }
                }
                hir::synthesis::HirNodeContext::Regular => {
                    // Resolve the full path
                    match resolve_path(db, *path_id, scope, true) {
                        Ok(path_res) => {
                            if let Some(scope) = path_res.as_scope(db) {
                                return Ok(vec![scope]);
                            }
                        }
                        Err(_) => {}
                    }
                }
                hir::synthesis::HirNodeContext::FieldAccess => {
                    // Field access context for path expression - shouldn't happen
                    // but we need to handle it for exhaustiveness
                }
            }

            Ok(vec![])
        }

        // Function calls - resolve to the function definition
        Partial::Present(Expr::Call(callee_expr_id, _args)) => {
            // Recursively resolve the callee expression
            // For function calls, we don't need segment resolution
            resolve_expression_definition(
                db,
                body,
                *callee_expr_id,
                hir::synthesis::HirNodeContext::Regular,
            )
        }

        // Method calls - resolve to the method definition
        Partial::Present(Expr::MethodCall(
            _receiver_expr_id,
            _method_name,
            _generic_args,
            _call_args,
        )) => {
            // For method calls, we need type information of the receiver
            // This is more complex and would involve type inference
            // For now, we'll leave this as a TODO and focus on path resolution
            // TODO: Implement method resolution using type inference
            Ok(vec![])
        }

        // Record initialization - resolve to the struct/record type definition
        Partial::Present(Expr::RecordInit(Partial::Present(path_id), _fields)) => {
            let scope = body.scope();

            // Resolve the record type path
            match resolve_path(db, *path_id, scope, false) {
                Ok(path_res) => {
                    if let Some(scope) = path_res.as_scope(db) {
                        return Ok(vec![scope]);
                    }
                    Ok(vec![])
                }
                Err(_) => Ok(vec![]),
            }
        }

        // Field access - resolve to the field definition
        Partial::Present(Expr::Field(_receiver_expr_id, field_index)) => {
            // We need to get the type of the receiver and find the field
            if let Some(_field_idx) = field_index.to_opt() {
                // TODO: Implement proper field resolution
                // This requires accessing type information and resolving to field definitions
                // which involves private APIs in hir-analysis
                // For now, field goto is not fully implemented
            }
            Ok(vec![])
        }

        // Other expression types don't have definitions to jump to
        _ => Ok(vec![]),
    }
}

/// Resolve definition location for a statement HIR node
fn resolve_statement_definition<'db>(
    db: &'db DriverDataBase,
    body: hir::hir_def::Body<'db>,
    stmt_id: hir::hir_def::StmtId,
    context: hir::synthesis::HirNodeContext,
) -> Result<Vec<ScopeId<'db>>, Box<dyn std::error::Error>> {
    use hir::hir_def::{Partial, Stmt};

    // Get the actual statement data from its ID
    let stmt_data = &body.stmts(db)[stmt_id];

    match stmt_data {
        // Let statements: resolve the type annotation or initializer expression
        Partial::Present(Stmt::Let(_pat_id, type_annotation, init_expr)) => {
            let mut scopes = vec![];

            // If there's a type annotation, try to resolve it
            if let Some(type_id) = type_annotation {
                // Get the type data and check if it's a path type
                if let hir::hir_def::TypeKind::Path(Partial::Present(path_id)) = type_id.data(db) {
                    // Resolve the path in the type annotation
                    let scope = body.scope();

                    // Check if we have segment context
                    match context {
                        hir::synthesis::HirNodeContext::PathSegment(segment_index) => {
                            // Resolve just the specific segment
                            if let Some(segment_path_id) = path_id.segment(db, segment_index) {
                                match resolve_path(db, segment_path_id, scope, false) {
                                    Ok(path_res) => {
                                        if let Some(scope) = path_res.as_scope(db) {
                                            scopes.push(scope);
                                        }
                                    }
                                    Err(_) => {}
                                }
                            }
                        }
                        hir::synthesis::HirNodeContext::Regular => {
                            // Resolve the full path
                            match resolve_path(db, *path_id, scope, false) {
                                Ok(path_res) => {
                                    if let Some(scope) = path_res.as_scope(db) {
                                        scopes.push(scope);
                                    }
                                }
                                Err(_) => {}
                            }
                        }
                        hir::synthesis::HirNodeContext::FieldAccess => {
                            // Field access in type annotation - not applicable here
                        }
                    }
                }
            }

            // If there's an initializer expression, resolve it
            if let Some(expr_id) = init_expr {
                let expr_scopes = resolve_expression_definition(
                    db,
                    body,
                    *expr_id,
                    hir::synthesis::HirNodeContext::Regular,
                )?;
                scopes.extend(expr_scopes);
            }

            Ok(scopes)
        }

        // For loops: resolve the iterable expression
        Partial::Present(Stmt::For(_pat_id, iterable_expr_id, _body_expr_id)) => {
            resolve_expression_definition(
                db,
                body,
                *iterable_expr_id,
                hir::synthesis::HirNodeContext::Regular,
            )
        }

        // While loops: resolve the condition expression
        Partial::Present(Stmt::While(condition_expr_id, _body_expr_id)) => {
            resolve_expression_definition(
                db,
                body,
                *condition_expr_id,
                hir::synthesis::HirNodeContext::Regular,
            )
        }

        // Expression statements: resolve the expression
        Partial::Present(Stmt::Expr(expr_id)) => resolve_expression_definition(
            db,
            body,
            *expr_id,
            hir::synthesis::HirNodeContext::Regular,
        ),

        // Other statement types don't have meaningful definitions
        _ => Ok(vec![]),
    }
}

/// Resolve definition location for a pattern HIR node
fn resolve_pattern_definition<'db>(
    db: &'db DriverDataBase,
    body: hir::hir_def::Body<'db>,
    pat_id: hir::hir_def::PatId,
) -> Result<Vec<ScopeId<'db>>, Box<dyn std::error::Error>> {
    use hir::hir_def::{Partial, Pat};

    // Get the actual pattern data from its ID
    let pat_data = &body.pats(db)[pat_id];

    match pat_data {
        // Path patterns: resolve to the type or variant definition
        Partial::Present(Pat::Path(Partial::Present(path_id), _is_mut)) => {
            let scope = body.scope();

            // Resolve the path to its definition
            match resolve_path(db, *path_id, scope, false) {
                Ok(path_res) => {
                    if let Some(scope) = path_res.as_scope(db) {
                        return Ok(vec![scope]);
                    }
                    Ok(vec![])
                }
                Err(_) => Ok(vec![]),
            }
        }

        // Path tuple patterns: resolve to the tuple type definition
        Partial::Present(Pat::PathTuple(Partial::Present(path_id), _tuple_pats)) => {
            let scope = body.scope();

            match resolve_path(db, *path_id, scope, false) {
                Ok(path_res) => {
                    if let Some(scope) = path_res.as_scope(db) {
                        return Ok(vec![scope]);
                    }
                    Ok(vec![])
                }
                Err(_) => Ok(vec![]),
            }
        }

        // Record patterns: resolve to the record type definition
        Partial::Present(Pat::Record(Partial::Present(path_id), _record_fields)) => {
            let scope = body.scope();

            match resolve_path(db, *path_id, scope, false) {
                Ok(path_res) => {
                    if let Some(scope) = path_res.as_scope(db) {
                        return Ok(vec![scope]);
                    }
                    Ok(vec![])
                }
                Err(_) => Ok(vec![]),
            }
        }

        // Other pattern types don't have definitions to jump to
        _ => Ok(vec![]),
    }
}

/// Performance comparison between old visitor-based and new HIR synthesis approaches
pub fn benchmark_goto_approaches<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> (std::time::Duration, std::time::Duration) {
    // Benchmark old visitor-based approach
    let start = std::time::Instant::now();
    let _old_result = get_goto_target_scopes_for_cursor(db, top_mod, cursor);
    let old_time = start.elapsed();

    // Benchmark new HIR synthesis approach
    let start = std::time::Instant::now();
    let _new_result = goto_definition_with_hir_synthesis(db, top_mod, cursor);
    let new_time = start.elapsed();

    (old_time, new_time)
}

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Convert the position to an offset in the file
    let params = params.text_document_position_params;
    let file_text = std::fs::read_to_string(params.text_document.uri.path()).ok();
    let cursor: Cursor = to_offset_from_position(params.position, file_text.unwrap().as_str());

    // Get the module and the goto info
    let file_path_str = params.text_document.uri.path();
    let url = url::Url::from_file_path(file_path_str).map_err(|()| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            format!("Invalid file path: {file_path_str}"),
        )
    })?;
    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found in workspace: {url}"),
            )
        })?;
    let top_mod = map_file_to_mod(&backend.db, file);

    // Use enhanced HIR synthesis approach
    match goto_definition_with_hir_synthesis(&backend.db, top_mod, cursor) {
        Ok(scopes) => {
            let locations: Result<Vec<_>, _> = scopes
                .iter()
                .map(|scope| crate::util::to_lsp_location_from_scope(&backend.db, *scope))
                .collect();
            match locations {
                Ok(locations) => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
                    locations,
                ))),
                Err(e) => {
                    error!("Failed to convert scopes to locations: {:?}", e);
                    Ok(None)
                }
            }
        }
        Err(e) => {
            error!("Enhanced goto definition failed: {:?}", e);
            Ok(None)
        }
    }
}
// }
#[cfg(test)]
mod tests {
    use common::ingot::IngotKind;
    use dir_test::{dir_test, Fixture};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;

    // given a cursor position and a string, convert to cursor line and column
    fn line_col_from_cursor(cursor: Cursor, s: &str) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;
        for (i, c) in s.chars().enumerate() {
            if i == Into::<usize>::into(cursor) {
                return (line, col);
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn extract_multiple_cursor_positions_from_spans(
        db: &DriverDataBase,
        top_mod: TopLevelMod,
    ) -> Vec<parser::TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        let mut path_collector = PathSpanCollector::default();
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        debug!(
            "PathSpanCollector found {} paths",
            path_collector.paths.len()
        );

        let mut cursors = Vec::new();
        for (path, _, lazy_span) in path_collector.paths {
            for idx in 0..=path.segment_index(db) {
                let seg_span = lazy_span.clone().segment(idx).resolve(db).unwrap();
                cursors.push(seg_span.range.start());
            }
        }

        cursors.sort();
        cursors.dedup();

        debug!("Found {} unique cursors: {:?}", cursors.len(), cursors);
        cursors
    }

    fn make_goto_cursors_snapshot(
        db: &DriverDataBase,
        fixture: &Fixture<&str>,
        top_mod: TopLevelMod,
    ) -> String {
        // Parse the file to get the syntax tree
        use parser::{ast, SyntaxKind, SyntaxNode};
        let green_node = hir::lower::parse_file_impl(db, top_mod);
        let root_syntax = SyntaxNode::new_root(green_node);
        let _content = fixture.content();
        let mut test_positions = Vec::new();

        // Walk the syntax tree to find all identifiers and paths
        fn collect_identifier_positions(node: &SyntaxNode, positions: &mut Vec<Cursor>) {
            use parser::ast::prelude::AstNode;

            // Check if this is an identifier or a path
            match node.kind() {
                SyntaxKind::Ident => {
                    // This is an identifier - test goto at its position
                    let start = node.text_range().start();
                    positions.push(start);
                }
                SyntaxKind::Path => {
                    // For paths, test each segment
                    if let Some(path) = ast::Path::cast(node.clone()) {
                        for segment in path.segments() {
                            if let Some(ident) = segment.ident() {
                                let start = ident.text_range().start();
                                positions.push(start);
                            }
                        }
                    }
                }
                SyntaxKind::PathType => {
                    // Type paths
                    if let Some(path_type) = ast::PathType::cast(node.clone()) {
                        if let Some(path) = path_type.path() {
                            for segment in path.segments() {
                                if let Some(ident) = segment.ident() {
                                    let start = ident.text_range().start();
                                    positions.push(start);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }

            // Recurse to children
            for child in node.children() {
                collect_identifier_positions(&child, positions);
            }
        }

        collect_identifier_positions(&root_syntax, &mut test_positions);

        test_positions.sort();
        test_positions.dedup();

        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();
        debug!(
            "Testing {} positions for goto resolution",
            test_positions.len()
        );
        let mut success_count = 0;
        let mut error_count = 0;

        for cursor in &test_positions {
            match goto_definition_with_hir_synthesis(db, top_mod, *cursor) {
                Ok(scopes) => {
                    if !scopes.is_empty() {
                        success_count += 1;
                        cursor_path_map.insert(
                            *cursor,
                            scopes
                                .iter()
                                .flat_map(|x| x.pretty_path(db))
                                .collect::<Vec<_>>()
                                .join("\n"),
                        );
                    }
                }
                Err(e) => {
                    error_count += 1;
                    debug!("Error at cursor {:?}: {:?}", cursor, e);
                }
            }
        }

        debug!(
            "Goto resolution: {} successful, {} errors, {} no results",
            success_count,
            error_count,
            test_positions.len() - success_count - error_count
        );

        let cursor_lines = cursor_path_map
            .iter()
            .map(|(cursor, path)| {
                let (cursor_line, cursor_col) = line_col_from_cursor(*cursor, fixture.content());
                format!("cursor position ({cursor_line:?}, {cursor_col:?}), path: {path}")
            })
            .collect::<Vec<_>>();

        format!(
            "{}\n---\n{}",
            fixture
                .content()
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{i:?}: {line}"))
                .collect::<Vec<_>>()
                .join("\n"),
            cursor_lines.join("\n")
        )
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/single_ingot",
        glob: "**/lib.fe",
    )]
    fn test_goto_multiple_files(fixture: Fixture<&str>) {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir =
            std::path::Path::new(&cargo_manifest_dir).join("test_files/single_ingot");

        let mut db = DriverDataBase::default();

        // Load all files from the ingot directory
        load_ingot_from_directory(&mut db, &ingot_base_dir);

        // Get our specific test file
        let fe_source_path = fixture.path();
        let file_url = Url::from_file_path(fe_source_path).unwrap();

        // Get the containing ingot - should be Local now
        let ingot = db.workspace().containing_ingot(&db, file_url).unwrap();
        assert_eq!(ingot.kind(&db), IngotKind::Local);

        // Introduce a new scope to limit the lifetime of `top_mod`
        {
            // Get the file directly from the file index
            let file_url = Url::from_file_path(fe_source_path).unwrap();
            let file = db.workspace().get(&db, &file_url).unwrap();
            let top_mod = map_file_to_mod(&db, file);

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
            snap_test!(snapshot, fixture.path());
        }

        // Get the containing ingot for the file path
        let file_url = Url::from_file_path(fixture.path()).unwrap();
        let ingot = db.workspace().containing_ingot(&db, file_url);
        assert_eq!(ingot.unwrap().kind(&db), IngotKind::Local);
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "smallest_enclosing*.fe"
    )]
    fn test_find_path_surrounding_cursor(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut

        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);

        let mut cursor_paths: Vec<(Cursor, String)> = vec![];

        for cursor in &cursors {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
            let mut path_collector = PathSpanCollector::default();
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let full_paths = path_collector.paths;

            if let Some((path, _, scope)) = find_path_surrounding_cursor(&db, *cursor, full_paths) {
                let resolved_enclosing_path = resolve_path(&db, path, scope, false);

                let res = match resolved_enclosing_path {
                    Ok(res) => res.pretty_path(&db).unwrap(),
                    Err(err) => match err.kind {
                        PathResErrorKind::Ambiguous(vec) => vec
                            .iter()
                            .map(|r| r.pretty_path(&db).unwrap())
                            .collect::<Vec<_>>()
                            .join("\n"),
                        _ => "".into(),
                    },
                };
                cursor_paths.push((*cursor, res));
            }
        }

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_paths
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {cursor:?}, path: {path}") })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }
}
