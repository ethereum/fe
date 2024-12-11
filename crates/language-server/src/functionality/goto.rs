use async_lsp::ResponseError;
use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind, PathId, TopLevelMod},
    span::DynLazySpan,
    visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt},
    LowerHirDb, SpannedHirDb,
};
use hir_analysis::name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameRes};

use crate::{
    backend::{db::LanguageServerDb, Backend},
    util::{to_lsp_location_from_scope, to_offset_from_position},
};
use hir::span::LazySpan;

pub type Cursor = rowan::TextSize;

#[derive(Default)]
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

fn find_path_surrounding_cursor<'db>(
    db: &'db dyn LanguageServerDb,
    cursor: Cursor,
    full_paths: Vec<(PathId<'db>, ScopeId<'db>, LazyPathSpan<'db>)>,
) -> Option<(PathId<'db>, bool, ScopeId<'db>)> {
    let hir_db = db.as_hir_db();
    for (path, scope, lazy_span) in full_paths {
        let span = lazy_span.resolve(db.as_spanned_hir_db()).unwrap();
        if span.range.contains(cursor) {
            for idx in 0..=path.segment_index(hir_db) {
                let seg_span = lazy_span
                    .segment(idx)
                    .resolve(db.as_spanned_hir_db())
                    .unwrap();
                if seg_span.range.contains(cursor) {
                    return Some((
                        path.segment(hir_db, idx).unwrap(),
                        idx != path.segment_index(hir_db),
                        scope,
                    ));
                }
            }
        }
    }
    return None;
}

pub fn find_enclosing_item<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<ItemKind<'db>> {
    let items = top_mod
        .scope_graph(db.as_hir_db())
        .items_dfs(db.as_hir_db());

    let mut smallest_enclosing_item = None;
    let mut smallest_range_size = None;

    for item in items {
        let lazy_item_span = DynLazySpan::from(item.lazy_span());
        let item_span = lazy_item_span
            .resolve(SpannedHirDb::as_spanned_hir_db(db))
            .unwrap();

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
    db: &'db dyn LanguageServerDb,
    top_mod: TopLevelMod<'db>,
    cursor: Cursor,
) -> Option<Vec<ScopeId<'db>>> {
    let item: ItemKind = find_enclosing_item(db.as_spanned_hir_db(), top_mod, cursor)?;

    let mut visitor_ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    let mut path_segment_collector = PathSpanCollector::default();
    path_segment_collector.visit_item(&mut visitor_ctxt, item);

    let (path, is_intermediate, scope) =
        find_path_surrounding_cursor(db, cursor, path_segment_collector.paths)?;

    let resolved = resolve_path_early(db.as_hir_analysis_db(), path, scope)?;

    let scopes = match resolved {
        EarlyResolvedPath::Full(bucket) => {
            if is_intermediate {
                bucket
                    .pick(NameDomain::TYPE)
                    .iter()
                    .flat_map(|res| res.scope())
                    .collect::<Vec<_>>()
            } else {
                bucket
                    .iter_ok()
                    .filter_map(NameRes::scope)
                    .collect::<Vec<_>>()
            }
        }
        EarlyResolvedPath::Partial { path: _, res } => {
            res.scope().iter().cloned().collect::<Vec<_>>()
        }
    };

    Some(scopes)
}

use crate::backend::workspace::IngotFileContext;

pub async fn handle_goto_definition(
    backend: &mut Backend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    // Convert the position to an offset in the file
    let params = params.text_document_position_params;
    let file_text = std::fs::read_to_string(params.text_document.uri.path()).ok();
    let cursor: Cursor = to_offset_from_position(params.position, file_text.unwrap().as_str());

    // Get the module and the goto info
    let file_path = params.text_document.uri.path();
    let top_mod = backend
        .workspace
        .top_mod_from_file_path(backend.db.as_lower_hir_db(), file_path)
        .unwrap();

    let scopes =
        get_goto_target_scopes_for_cursor(&backend.db, top_mod, cursor).unwrap_or_default();

    let locations = scopes
        .iter()
        .map(|scope| to_lsp_location_from_scope(*scope, backend.db.as_spanned_hir_db()))
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
            eprintln!("Error handling goto definition: {:?}", e);
            None
        }
    };
    Ok(response)
}
// }
#[cfg(test)]
mod tests {
    use crate::backend::{
        db::LanguageServerDatabase,
        workspace::{IngotFileContext, Workspace},
    };

    use super::*;
    use common::input::IngotKind;
    use dir_test::{dir_test, Fixture};
    use fe_compiler_test_utils::snap_test;
    use fxhash::FxHashMap;
    use hir::{HirDb, LowerHirDb};
    use std::{collections::BTreeMap, path::Path};

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
        db: &LanguageServerDatabase,
        top_mod: TopLevelMod,
    ) -> Vec<rowan::TextSize> {
        let hir_db = db.as_hir_db();
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(hir_db, top_mod);
        let mut path_collector = PathSpanCollector::default();
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let mut cursors = Vec::new();
        for (path, _, lazy_span) in path_collector.paths {
            for idx in 0..=path.segment_index(hir_db) {
                let seg_span = lazy_span
                    .segment(idx)
                    .resolve(db.as_spanned_hir_db())
                    .unwrap();
                cursors.push(seg_span.range.start());
            }
        }

        cursors.sort();
        cursors.dedup();

        eprintln!("Found cursors: {:?}", cursors);
        cursors
    }

    fn make_goto_cursors_snapshot(
        db: &LanguageServerDatabase,
        fixture: &Fixture<&str>,
        top_mod: TopLevelMod,
    ) -> String {
        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);
        let mut cursor_path_map: BTreeMap<Cursor, String> = BTreeMap::default();

        for cursor in &cursors {
            let scopes =
                get_goto_target_scopes_for_cursor(db, top_mod, *cursor).unwrap_or_default();

            if !scopes.is_empty() {
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
        let ingot_base_dir = Path::new(&cargo_manifest_dir).join("test_files/single_ingot");

        let mut db = LanguageServerDatabase::default();
        let mut workspace = Workspace::default();

        let _ = workspace.set_workspace_root(&mut db, &ingot_base_dir);

        let fe_source_path = ingot_base_dir.join(fixture.path());
        let fe_source_path = fe_source_path.to_str().unwrap();
        let input = workspace.touch_input_for_file_path(&mut db, fixture.path());
        assert_eq!(input.unwrap().ingot(&db).kind(&db), IngotKind::Local);

        input
            .unwrap()
            .set_text(&mut db)
            .to((*fixture.content()).to_string());

        // Introduce a new scope to limit the lifetime of `top_mod`
        {
            let top_mod = workspace
                .top_mod_from_file_path(db.as_lower_hir_db(), fe_source_path)
                .unwrap();

            let snapshot = make_goto_cursors_snapshot(&db, &fixture, top_mod);
            snap_test!(snapshot, fixture.path());
        }

        let ingot = workspace.touch_ingot_for_file_path(&mut db, fixture.path());
        assert_eq!(ingot.unwrap().kind(&db), IngotKind::Local);
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_cursor_target(fixture: Fixture<&str>) {
        let db = &mut LanguageServerDatabase::default();
        let workspace = &mut Workspace::default();
        let input = workspace
            .touch_input_for_file_path(db, fixture.path())
            .unwrap();
        input.set_text(db).to((*fixture.content()).to_string());
        let top_mod = workspace
            .top_mod_from_file_path(db.as_lower_hir_db(), fixture.path())
            .unwrap();

        let snapshot = make_goto_cursors_snapshot(db, &fixture, top_mod);
        snap_test!(snapshot, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "smallest_enclosing*.fe"
    )]
    fn test_find_path_surrounding_cursor(fixture: Fixture<&str>) {
        let db = &mut LanguageServerDatabase::default();
        let workspace = &mut Workspace::default();

        workspace
            .touch_input_for_file_path(db, fixture.path())
            .unwrap()
            .set_text(db)
            .to((*fixture.content()).to_string());
        let top_mod = workspace
            .top_mod_from_file_path(db.as_lower_hir_db(), fixture.path())
            .unwrap();

        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);

        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        for cursor in &cursors {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
            let mut path_collector = PathSpanCollector::default();
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let full_paths = path_collector.paths;

            if let Some((path, _, scope)) = find_path_surrounding_cursor(db, *cursor, full_paths) {
                let resolved_enclosing_path =
                    hir_analysis::name_resolution::resolve_path_early(db, path, scope).unwrap();

                let res = match resolved_enclosing_path {
                    EarlyResolvedPath::Full(bucket) => bucket
                        .iter_ok()
                        .map(|x| x.pretty_path(db).unwrap())
                        .collect::<Vec<_>>()
                        .join("\n"),
                    EarlyResolvedPath::Partial { res, path: _ } => res.pretty_path(db).unwrap(),
                };
                cursor_path_map.insert(*cursor, res);
            }
        }

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_path_map
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {cursor:?}, path: {path}") })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }
}
