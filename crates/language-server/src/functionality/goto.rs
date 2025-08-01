use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::ItemKind,
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::error;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_scope, to_lsp_location_from_lazy_span, to_offset_from_position},
};
use driver::DriverDataBase;
pub type Cursor = parser::TextSize;

/// Convert a Definition to an LSP Location
fn definition_to_lsp_location(
    db: &DriverDataBase,
    definition: &hir_analysis::tooling_api::Definition,
) -> Result<async_lsp::lsp_types::Location, Box<dyn std::error::Error>> {
    match definition {
        hir_analysis::tooling_api::Definition::Scope(scope) => {
            to_lsp_location_from_scope(db, *scope)
        }
        hir_analysis::tooling_api::Definition::Location(lazy_span) => {
            to_lsp_location_from_lazy_span(db, lazy_span.clone())
        }
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
    
    

    // Get definitions for the cursor position
    let mut definitions = hir_analysis::tooling_api::get_goto_definitions(&backend.db, top_mod, cursor).unwrap_or_default();
    
    // Debug logging
    eprintln!("Goto definition at cursor {:?}: found {} definitions", cursor, definitions.len());
    for (i, def) in definitions.iter().enumerate() {
        match def {
            hir_analysis::tooling_api::Definition::Scope(scope) => {
                eprintln!("  Definition {}: Scope - {:?}", i, scope.pretty_path(&backend.db));
            }
            hir_analysis::tooling_api::Definition::Location(_span) => {
                eprintln!("  Definition {}: Location span", i);
            }
        }
    }
    
    // If we didn't find any definitions, check if this cursor is at an item definition name
    if definitions.is_empty() {
            use hir::span::item::{LazyStructSpan, LazyFuncSpan, LazyEnumSpan};
            use hir::hir_def::scope_graph::ScopeId;
            
            let items = top_mod.scope_graph(&backend.db).items_dfs(&backend.db);
            for item in items {
                match item {
                    ItemKind::Struct(s) => {
                        let struct_span = LazyStructSpan::new(s);
                        if let Some(name_span) = struct_span.name().resolve(&backend.db) {
                            if name_span.range.start() == cursor {
                                definitions = vec![hir_analysis::tooling_api::Definition::Scope(ScopeId::from_item(item))];
                                break;
                            }
                        }
                    }
                    ItemKind::Func(f) => {
                        let func_span = LazyFuncSpan::new(f);
                        if let Some(name_span) = func_span.name().resolve(&backend.db) {
                            if name_span.range.start() == cursor {
                                definitions = vec![hir_analysis::tooling_api::Definition::Scope(ScopeId::from_item(item))];
                                break;
                            }
                        }
                    }
                    ItemKind::Enum(e) => {
                        let enum_span = LazyEnumSpan::new(e);
                        if let Some(name_span) = enum_span.name().resolve(&backend.db) {
                            if name_span.range.start() == cursor {
                                definitions = vec![hir_analysis::tooling_api::Definition::Scope(ScopeId::from_item(item))];
                                break;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    
    let locations = definitions
        .iter()
        .map(|def| definition_to_lsp_location(&backend.db, def))
        .collect::<Vec<_>>();

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
    use hir::hir_def::TopLevelMod;
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
            match &position {
                hir_analysis::tooling_api::ResolvablePosition::Path(path, _, lazy_span) => {
                    // Extract cursor positions from all path segments
                    for idx in 0..=path.segment_index(db) {
                        if let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) {
                            // Add multiple cursor positions across the span for better test coverage
                            cursors.push(seg_span.range.start());
                            if seg_span.range.end() > seg_span.range.start() + parser::TextSize::from(1) {
                                cursors.push(seg_span.range.start() + parser::TextSize::from(1));
                            }
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::FieldAccess(_, _, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::MethodCall(_, _, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::PatternField(_, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::LocalBinding(_ident, _scope, lazy_span) => {
                    // For local variables, we need the span of the variable usage, not definition
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::FieldDefinition(_, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::VariantDefinition(_, _, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
                    }
                }
                hir_analysis::tooling_api::ResolvablePosition::ItemDefinition(_, lazy_span) => {
                    if let Some(span) = lazy_span.resolve(db) {
                        cursors.push(span.range.start());
                        // Add multiple cursors to test that goto works across the entire span
                        if span.range.len() > parser::TextSize::from(2) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                            cursors.push(span.range.end() - parser::TextSize::from(1));
                        } else if span.range.len() > parser::TextSize::from(1) {
                            cursors.push(span.range.start() + parser::TextSize::from(1));
                        }
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
        
        for (_idx, cursor) in cursors.iter().enumerate() {
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