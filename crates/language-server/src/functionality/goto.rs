use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::{
        scope_graph::ScopeId, ItemKind, PathId, TopLevelMod,
    },
    lower::map_file_to_mod,
};
use tracing::error;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_scope, to_lsp_location_from_lazy_span, to_offset_from_position},
};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

pub fn get_goto_target_scopes_for_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    // Use the new consolidated tooling API
    hir_analysis::tooling_api::get_goto_definition_scopes(db, top_mod, cursor)
}

/// Get precise local variable definition locations using direct span information
fn get_precise_local_variable_locations(
    db: &DriverDataBase,
    top_mod: TopLevelMod,
    cursor: Cursor,
) -> Option<Vec<Result<async_lsp::lsp_types::Location, Box<dyn std::error::Error>>>> {
    // Find the enclosing function
    let enclosing_item = hir_analysis::tooling_api::find_enclosing_item(db, top_mod, cursor)?;
    let func = match enclosing_item {
        ItemKind::Func(func) => func,
        ItemKind::Body(body) => {
            let body_scope = ScopeId::from_item(ItemKind::Body(body));
            if let Some(parent_scope) = body_scope.parent(db) {
                if let ScopeId::Item(ItemKind::Func(parent_func)) = parent_scope {
                    parent_func
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // Find the resolvable position at cursor using tooling API
    let positions = hir_analysis::tooling_api::collect_resolvable_positions(db, top_mod);
    let position = hir_analysis::tooling_api::find_position_at_cursor(db, cursor, positions)?;
    
    match position {
        hir_analysis::tooling_api::ResolvablePosition::LocalVariable(ident, scope, _span) => {
            // Use the comprehensive resolution to get local bindings
            let path = PathId::from_ident(db, ident);
            let resolution = hir_analysis::tooling_api::resolve_identifier_comprehensive(
                db, path, scope, Some(func)
            );
            
            let mut precise_locations = Vec::new();
            let body = func.body(db)?;
            
            // For each local binding, get its definition span directly
            for binding in &resolution.local_bindings {
                if let Some(def_span) = binding.definition_span(db, body) {
                    let location = to_lsp_location_from_lazy_span(db, def_span);
                    precise_locations.push(location);
                }
            }
            
            if !precise_locations.is_empty() {
                Some(precise_locations)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Get the file from the database first
    let params = params.text_document_position_params;
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
                format!("File not found in index: {url} (original path: {file_path_str})"),
            )
        })?;

    // Use the database content (which includes any unsaved changes) for cursor position
    let file_text = file.text(&backend.db);
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Check if we can get precise locations for local variables using spans
    let locations = if let Some(precise_locations) = get_precise_local_variable_locations(&backend.db, top_mod, cursor) {
        // For local variables, use the precise span-based locations
        precise_locations
    } else {
        // For everything else, use the scope-based approach
        let scopes = get_goto_target_scopes_for_cursor(&backend.db, top_mod, cursor).unwrap_or_default();
        scopes
            .iter()
            .map(|scope| to_lsp_location_from_scope(&backend.db, *scope))
            .collect::<Vec<_>>()
    };

    let result: Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ()> =
        Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locations
                .into_iter()
                .filter_map(std::result::Result::ok)
                .collect(),
        )));
    let response = match result {
        Ok(response) => response,
        Err(e) => {
            error!("Error handling goto definition: {:?}", e);
            None
        }
    };
    Ok(response)
}

#[cfg(test)]
mod tests {
    use dir_test::{dir_test, Fixture};
    use std::collections::BTreeMap;
    use test_utils::snap_test;
    use url::Url;

    use super::*;

    fn line_col_from_cursor(cursor: Cursor, src: &str) -> (usize, usize) {
        let cursor_pos: usize = cursor.into();
        let mut line = 1;
        let mut col = 1;
        for (i, c) in src.chars().enumerate() {
            if i == cursor_pos {
                return (line, col);
            }
            if c == '\n' {
                line += 1;
                col = 1;
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
        let positions = hir_analysis::tooling_api::collect_resolvable_positions(db, top_mod);
        let mut cursors = Vec::new();
        
        for position in positions {
            match position {
                hir_analysis::tooling_api::ResolvablePosition::Path(path, _, lazy_span) => {
                    // Extract cursor positions from all path segments
                    for idx in 0..=path.segment_index(db) {
                        if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                            cursors.push(seg_span.range.start());
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::FieldAccess(_, _, _, lazy_span) |
                hir_analysis::tooling_api::ResolvablePosition::MethodCall(_, _, _, lazy_span) |
                hir_analysis::tooling_api::ResolvablePosition::LocalVariable(_, _, lazy_span) |
                hir_analysis::tooling_api::ResolvablePosition::PatternField(_, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                    }
                }
            }
        }

        cursors.sort();
        cursors.dedup();
        cursors
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);
        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();

        for cursor in &cursors {
            let scopes = hir_analysis::tooling_api::get_goto_definition_scopes(&db, top_mod, *cursor).unwrap_or_default();

            if !scopes.is_empty() {
                cursor_path_map.insert(
                    *cursor,
                    scopes
                        .iter()
                        .flat_map(|x| x.pretty_path(&db))
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
            }
        }

        let cursor_lines = cursor_path_map
            .iter()
            .map(|(cursor, path)| {
                let (cursor_line, cursor_col) = line_col_from_cursor(*cursor, fixture.content());
                format!("cursor position ({cursor_line:?}, {cursor_col:?}), path: {path}")
            })
            .collect::<Vec<_>>();

        let snapshot = format!(
            "{}\n---\n{}",
            fixture
                .content()
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{i:?}: {line}"))
                .collect::<Vec<_>>()
                .join("\n"),
            cursor_lines.join("\n")
        );
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto_values.fe",
    )]
    fn test_goto_values(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let mut cursor_paths = Vec::new();
        let cursors = extract_multiple_cursor_positions_from_spans(&db, top_mod);
        
        for cursor in cursors {
            if let Some(scopes) = hir_analysis::tooling_api::get_goto_definition_scopes(&db, top_mod, cursor) {
                let path_results: Vec<_> = scopes
                    .iter()
                    .flat_map(|scope| scope.pretty_path(&db))
                    .collect();
                if !path_results.is_empty() {
                    cursor_paths.push((cursor, path_results.join("\n")));
                }
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