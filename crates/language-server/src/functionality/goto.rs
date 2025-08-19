use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::{scope_graph::ScopeId, TopLevelMod},
    lower::map_file_to_mod,
    SpannedHirDb,
};
use hir_analysis::navigation::{resolve_goto_full, NavTarget};

use tracing::{debug, error};

use crate::{backend::Backend, util::to_offset_from_position};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

#[derive(Debug)]
pub enum GotoTarget<'db> {
    Scope(ScopeId<'db>),
    Span(common::diagnostics::Span),
}

impl<'db> GotoTarget<'db> {
    pub fn pretty_path(&self, db: &dyn SpannedHirDb) -> Option<String> {
        match self {
            GotoTarget::Scope(scope) => scope.pretty_path(db),
            GotoTarget::Span(span) => {
                if let Ok(l) = crate::util::to_lsp_location_from_span(db, span.clone()) {
                    Some(format!(
                        "local at {}:{}",
                        l.range.start.line, l.range.start.character
                    ))
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod debug_vis {
    use super::*;
    use hir_analysis::name_resolution::{resolve_path, PathResErrorKind};
    use hir::hir_def::{scope_graph::ScopeId, PathId};
    use hir::span::LazySpan;
    use hir::visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt};

    #[derive(Default)]
    pub struct PathSpanCollector<'db> {
        pub paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    }

    impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
        fn visit_path(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>,
            path: PathId<'db>,
        ) {
            let Some(span) = ctxt.span() else {
                return;
            };
            let scope = ctxt.scope();
            self.paths.push((path, scope, span));
        }
    }

    pub fn find_path_surrounding_cursor<'db>(
        db: &'db DriverDataBase,
        cursor: super::Cursor,
        full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
    ) -> Option<(PathId<'db>, bool, ScopeId<'db>)> {
        let mut best: Option<(
            PathId<'db>,
            ScopeId<'db>,
            LazyPathSpan<'db>,
            parser::TextRange,
        )> = None;
        for (path, scope, lazy_span) in full_paths {
            let span = lazy_span.resolve(db).unwrap();
            if span.range.contains(cursor) {
                let len = span.range.end() - span.range.start();
                if best
                    .as_ref()
                    .map(|(_, _, _, r)| (r.end() - r.start()) > len)
                    .unwrap_or(true)
                {
                    best = Some((path, scope, lazy_span, span.range));
                }
            }
        }
        let Some((path, scope, lazy_span, _)) = best.clone() else {
            return None;
        };
        for idx in 0..=path.segment_index(db) {
            let seg_lazy = lazy_span.clone().segment(idx);
            let seg_full = seg_lazy.resolve(db).unwrap();
            if seg_full.range.contains(cursor) {
                if let Some(id_span) = seg_lazy.ident().resolve(db) {
                    if id_span.range.contains(cursor) {
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
}

#[cfg(test)]
pub fn get_goto_target_scopes_for_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    use debug_vis::*;
    use hir::{
        hir_def::ItemKind,
        visitor::{Visitor, VisitorCtxt},
    };
    let item: ItemKind = hir::collect::find_enclosing_item_at(db, top_mod, cursor)?;
    let mut visitor_ctxt = VisitorCtxt::with_item(db, item);
    let mut path_segment_collector = PathSpanCollector::default();
    path_segment_collector.visit_item(&mut visitor_ctxt, item);
    let (path, _is_intermediate, scope) =
        find_path_surrounding_cursor(db, cursor, path_segment_collector.paths)?;
    let resolved = hir_analysis::name_resolution::resolve_path(db, path, scope, false);
    let scopes = match resolved {
        Ok(r) => r.as_scope(db).into_iter().collect::<Vec<_>>(),
        Err(err) => match err.kind {
            hir_analysis::name_resolution::PathResErrorKind::NotFound(bucket) => {
                bucket.iter_ok().flat_map(|r| r.scope()).collect()
            }
            hir_analysis::name_resolution::PathResErrorKind::Ambiguous(vec) => {
                vec.into_iter().flat_map(|r| r.scope()).collect()
            }
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
    file: common::file::File,
) -> Result<Vec<GotoTarget<'db>>, Box<dyn std::error::Error>> {
    use crate::hir_integration::{lazy_hir_for_cursor, LazyHirResult};

    // First check if the cursor is on an identifier character
    // This prevents goto from triggering on punctuation like ::, =, etc.
    if !is_cursor_on_identifier_char(db, cursor, file) {
        debug!("Cursor is not on an identifier character, skipping goto");
        return Ok(vec![]);
    }

    // Use the HIR synthesis system to find the HIR node at the cursor position
    let hir_result = lazy_hir_for_cursor(db, top_mod, cursor);

    debug!(
        "Cursor {:?}: HIR result = {:?}",
        cursor,
        match &hir_result {
            LazyHirResult::Expr(_, _, _) => "Expr",
            LazyHirResult::Stmt(_, _, _) => "Stmt",
            LazyHirResult::Pat(_, _) => "Pat",
            LazyHirResult::ItemPath(_, _, _, _) => "ItemPath",
            LazyHirResult::ItemType(_, _, _) => "ItemType",
            LazyHirResult::ItemGenericParam(_, _) => "ItemGenericParam",
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
        LazyHirResult::ItemPath(item, _path, ctx, seg) => {
            debug!(
                "HIR synthesis found item path: item={:?}, context={:?}, segment={:?}",
                item, ctx, seg
            );
        }
        LazyHirResult::ItemType(item, _ty, ctx) => {
            debug!(
                "HIR synthesis found item type: item={:?}, context={:?}",
                item, ctx
            );
        }
        LazyHirResult::ItemGenericParam(item, idx) => {
            debug!(
                "HIR synthesis found item generic param: item={:?}, idx={:?}",
                item, idx
            );
        }
        LazyHirResult::None => {
            debug!(
                "HIR synthesis found nothing at cursor position {:?}",
                cursor
            );
        }
    }

    // Resolve via hir-analysis navigation
    let nav_targets = resolve_goto_full(db, db, top_mod, cursor, hir_result);
    let result = Ok(nav_targets
        .into_iter()
        .map(|t| match t {
            NavTarget::Scope(s) => GotoTarget::Scope(s),
            NavTarget::Span(sp) => GotoTarget::Span(sp),
        })
        .collect());

    result
}

/// Check if cursor is on an identifier character
fn is_cursor_on_identifier_char<'db>(
    db: &'db DriverDataBase,
    cursor: Cursor,
    file: common::file::File,
) -> bool {
    let source = file.text(db);
    let cursor_offset: usize = cursor.into();

    // Special case: cursor is at end of file - check if previous char is identifier
    if cursor_offset == source.len() && cursor_offset > 0 {
        if let Some(ch) = source.chars().nth(cursor_offset - 1) {
            return ch.is_alphanumeric() || ch == '_';
        }
        return false;
    }

    if cursor_offset >= source.len() {
        return false;
    }

    // Get the character at the cursor position
    if let Some(ch) = source.chars().nth(cursor_offset) {
        return ch.is_alphanumeric() || ch == '_';
    }

    false
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
    match goto_definition_with_hir_synthesis(&backend.db, top_mod, cursor, file) {
        Ok(targets) => {
            if !targets.is_empty() {
                let locations: Result<Vec<_>, _> = targets
                    .iter()
                    .map(|target| match target {
                        GotoTarget::Scope(scope) => {
                            crate::util::to_lsp_location_from_scope(&backend.db, *scope)
                        }
                        GotoTarget::Span(ref span) => {
                            crate::util::to_lsp_location_from_span(&backend.db, span.clone())
                        }
                    })
                    .collect();
                match locations {
                    Ok(locations) => Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
                        locations,
                    ))),
                    Err(e) => {
                        error!("Failed to convert targets to locations: {:?}", e);
                        Ok(None)
                    }
                }
            } else {
                Ok(None)
            }
        }
        Err(e) => {
            error!("Enhanced goto definition failed: {:?}", e);
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use common::ingot::IngotKind;
    use dir_test::{dir_test, Fixture};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;
    pub use crate::functionality::goto::debug_vis::{
        find_path_surrounding_cursor, PathSpanCollector,
    };
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;
    use hir::visitor::{Visitor, VisitorCtxt};
    use hir::span::LazySpan;
    use hir_analysis::name_resolution::{resolve_path, PathResErrorKind};

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
        file: common::file::File,
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
            match goto_definition_with_hir_synthesis(db, top_mod, *cursor, file) {
                Ok(targets) => {
                    if !targets.is_empty() {
                        success_count += 1;
                        cursor_path_map.insert(
                            *cursor,
                            targets
                                .iter()
                                .filter_map(|x| x.pretty_path(db))
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

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod, file);
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

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod, file);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "test_local_goto.fe"
    )]
    fn test_local_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default(); // Changed to mut
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod, file);
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
