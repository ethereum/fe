use async_lsp::ResponseError;
use fxhash::FxHashMap;
use hir::{
    hir_def::{scope_graph::ScopeId, IdentId, ItemKind, Partial, PathId, TopLevelMod},
    span::DynLazySpan,
    visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt},
    LowerHirDb, SpannedHirDb,
};
use hir_analysis::name_resolution::{EarlyResolvedPath, NameDomain, NameRes};

use crate::{
    backend::{db::LanguageServerDb, Backend},
    util::{to_lsp_location_from_scope, to_offset_from_position},
};
use common::diagnostics::Span;
use hir::span::LazySpan;

pub type Cursor = rowan::TextSize;

#[derive(Clone, Copy)]
struct GotoEnclosingPathSegment<'db> {
    path: PathId<'db>,
    idx: usize,
    scope: ScopeId<'db>,
}

impl<'db> GotoEnclosingPathSegment<'db> {
    fn segments(self, db: &'db dyn LanguageServerDb) -> &'db [Partial<IdentId<'db>>] {
        &self.path.segments(db.as_hir_db())[0..self.idx + 1]
    }
    fn is_intermediate(self, db: &dyn LanguageServerDb) -> bool {
        self.idx < self.path.segments(db.as_hir_db()).len() - 1
    }
}

struct PathSegmentSpanCollector<'db> {
    segment_map: FxHashMap<Span, GotoEnclosingPathSegment<'db>>,
    db: &'db dyn LanguageServerDb,
}

impl<'db> PathSegmentSpanCollector<'db> {
    fn new(db: &'db dyn LanguageServerDb) -> Self {
        Self {
            segment_map: FxHashMap::default(),
            db,
        }
    }
}

impl<'db, 'ast: 'db> Visitor<'ast> for PathSegmentSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan>, path: PathId<'ast>) {
        let Some(path_span) = ctxt.span() else {
            return;
        };

        let scope = ctxt.scope();
        for i in 0..path.segments(self.db.as_hir_db()).len() {
            let Some(segment_span) = path_span.segment(i).resolve(self.db.as_spanned_hir_db())
            else {
                continue;
            };

            self.segment_map.insert(
                segment_span,
                GotoEnclosingPathSegment {
                    path,
                    idx: i,
                    scope,
                },
            );
        }
    }
}

fn smallest_enclosing_segment<'db>(
    cursor: Cursor,
    ident_map: &FxHashMap<Span, GotoEnclosingPathSegment<'db>>,
) -> Option<GotoEnclosingPathSegment<'db>> {
    let mut smallest_enclosing_segment = None;
    let mut smallest_range_size = None;

    for (span, &enclosing_segment) in ident_map {
        if span.range.contains(cursor) {
            let range_size = span.range.end() - span.range.start();
            if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                smallest_enclosing_segment = Some(enclosing_segment);
                smallest_range_size = Some(range_size);
            }
        }
    }

    smallest_enclosing_segment
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
    let mut path_segment_collector = PathSegmentSpanCollector::new(db);
    path_segment_collector.visit_item(&mut visitor_ctxt, item);

    let cursor_segment = smallest_enclosing_segment(cursor, &path_segment_collector.segment_map)?;

    let segments = cursor_segment.segments(db);
    let is_intermediate_segment = cursor_segment.is_intermediate(db);
    // let is_partial = cursor_segment.idx < cursor_segment.path.segments(db.as_jar_db()).len();
    let resolved_segments = hir_analysis::name_resolution::resolve_segments_early(
        db.as_hir_analysis_db(),
        segments,
        cursor_segment.scope,
    )?;

    let scopes = match resolved_segments {
        EarlyResolvedPath::Full(bucket) => {
            if is_intermediate_segment {
                match bucket.pick(NameDomain::TYPE) {
                    Ok(res) => res.scope().iter().cloned().collect::<Vec<_>>(),
                    _ => bucket
                        .iter_ok()
                        .filter_map(NameRes::scope)
                        .collect::<Vec<_>>(),
                }
            } else {
                bucket
                    .iter_ok()
                    .filter_map(NameRes::scope)
                    .collect::<Vec<_>>()
            }
        }
        EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        } => res.scope().iter().cloned().collect::<Vec<_>>(),
    };

    Some(scopes)
}

use crate::backend::workspace::IngotFileContext;

// impl Backend {
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
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
        // let mut path_collector = PathSpanCollector::new(db);
        let mut path_collector = PathSegmentSpanCollector::new(db);
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let segment_map = path_collector.segment_map;

        let mut cursors = Vec::new();
        for (span, _) in segment_map {
            let cursor = span.range.start();
            cursors.push(cursor);
        }

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
    fn test_smallest_enclosing_path(fixture: Fixture<&str>) {
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
            let mut path_collector = PathSegmentSpanCollector::new(db);
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let path_map = path_collector.segment_map;
            let enclosing_path_segment = smallest_enclosing_segment(*cursor, &path_map);

            if let Some(GotoEnclosingPathSegment { path, scope, .. }) = enclosing_path_segment {
                let resolved_enclosing_path =
                    hir_analysis::name_resolution::resolve_path_early(db, path, scope).unwrap();

                let res = match resolved_enclosing_path {
                    EarlyResolvedPath::Full(bucket) => bucket
                        .iter_ok()
                        .map(|x| x.pretty_path(db).unwrap())
                        .collect::<Vec<_>>()
                        .join("\n"),
                    EarlyResolvedPath::Partial {
                        res,
                        unresolved_from: _,
                    } => res.pretty_path(db).unwrap(),
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
