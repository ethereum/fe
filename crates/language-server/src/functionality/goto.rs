use fxhash::FxHashMap;
use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind, PathId, TopLevelMod},
    visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt},
    HirDb, LowerHirDb, SpannedHirDb,
};
use hir_analysis::{
    name_resolution::{EarlyResolvedPath, NameRes},
    HirAnalysisDb,
};
use salsa::Snapshot;

use crate::{
    backend::db::{LanguageServerDatabase, LanguageServerDb},
    util::{to_lsp_location_from_scope, to_offset_from_position},
};
use common::diagnostics::Span;
use hir::span::LazySpan;

pub type GotoEnclosingPath = (PathId, ScopeId);
pub type GotoPathMap = FxHashMap<Span, GotoEnclosingPath>;

pub struct PathSpanCollector<'db> {
    path_map: GotoPathMap,
    db: &'db dyn LanguageServerDb,
}

impl<'db> PathSpanCollector<'db> {
    pub fn new(db: &'db LanguageServerDatabase) -> Self {
        Self {
            path_map: FxHashMap::default(),
            db,
        }
    }
}

pub type Cursor = rowan::TextSize;

impl<'db> Visitor for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'_, LazyPathSpan>, path: PathId) {
        let Some(span) = ctxt
            .span()
            .and_then(|lazy_span| lazy_span.resolve(self.db.as_spanned_hir_db()))
        else {
            return;
        };

        let scope = ctxt.scope();
        self.path_map.insert(span, (path, scope));
    }
}

fn smallest_enclosing_path(cursor: Cursor, path_map: &GotoPathMap) -> Option<GotoEnclosingPath> {
    let mut smallest_enclosing_path = None;
    let mut smallest_range_size = None;

    for (span, enclosing_path) in path_map {
        if span.range.contains(cursor) {
            let range_size = span.range.end() - span.range.start();
            if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                smallest_enclosing_path = Some(*enclosing_path);
                smallest_range_size = Some(range_size);
            }
        }
    }

    smallest_enclosing_path
}

pub fn goto_enclosing_path(
    db: &Snapshot<LanguageServerDatabase>,
    top_mod: TopLevelMod,
    cursor: Cursor,
) -> Option<EarlyResolvedPath> {
    // Find the innermost item enclosing the cursor.
    let item: ItemKind = db.find_enclosing_item(top_mod, cursor)?;

    let mut visitor_ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    let mut path_collector = PathSpanCollector::new(db);
    path_collector.visit_item(&mut visitor_ctxt, item);

    // can we do this without the cache?
    let path_map = path_collector.path_map;

    // Find the path that encloses the cursor.
    let goto_starting_path = smallest_enclosing_path(cursor, &path_map)?;

    let (path_id, scope_id) = goto_starting_path;

    // Resolve path.
    let resolved_path = hir_analysis::name_resolution::resolve_path_early(
        db.as_hir_analysis_db(),
        path_id,
        scope_id,
    );

    Some(resolved_path)
}

use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse};

use crate::backend::workspace::{IngotFileContext, Workspace};
// use crate::diagnostics::get_diagnostics;

use std::sync::Arc;
use tokio::sync::RwLock;

use tower_lsp::jsonrpc::Result;

pub async fn goto_helper(
    db: Snapshot<LanguageServerDatabase>,
    workspace: Arc<RwLock<Workspace>>,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let workspace = workspace.read().await;
    // Convert the position to an offset in the file
    let params = params.text_document_position_params;
    let file_text = std::fs::read_to_string(params.text_document.uri.path()).ok();
    let cursor: Cursor = to_offset_from_position(params.position, file_text.unwrap().as_str());

    // Get the module and the goto info
    let file_path = params.text_document.uri.path();
    let top_mod = workspace
        .top_mod_from_file_path(db.as_lower_hir_db(), file_path)
        .unwrap();
    let goto_info = goto_enclosing_path(&db, top_mod, cursor);

    // Convert the goto info to a Location
    let scopes = match goto_info {
        Some(EarlyResolvedPath::Full(bucket)) => {
            bucket.iter().map(NameRes::scope).collect::<Vec<_>>()
        }
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => {
            vec![res.scope()]
        }
        None => return Ok(None),
    };

    let locations = scopes
        .iter()
        .filter_map(|scope| *scope)
        .map(|scope| to_lsp_location_from_scope(scope, db.as_spanned_hir_db()))
        .collect::<Vec<_>>();

    let _errors = scopes
        .iter()
        .filter_map(|scope| *scope)
        .map(|scope| to_lsp_location_from_scope(scope, db.as_spanned_hir_db()))
        .filter_map(std::result::Result::err)
        .map(|err| err.to_string())
        .collect::<Vec<_>>()
        .join("\n");

    Ok(Some(lsp_types::GotoDefinitionResponse::Array(
        locations
            .into_iter()
            .filter_map(std::result::Result::ok)
            .collect(),
    )))
}

#[cfg(test)]
mod tests {
    use crate::backend::workspace::{IngotFileContext, Workspace};

    use super::*;
    use common::input::IngotKind;
    use dir_test::{dir_test, Fixture};
    use fe_compiler_test_utils::snap_test;
    use hir::LowerHirDb;
    use salsa::ParallelDatabase;
    use std::path::Path;

    fn extract_multiple_cursor_positions_from_spans(
        db: &mut LanguageServerDatabase,
        top_mod: TopLevelMod,
    ) -> Vec<rowan::TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
        let mut path_collector = PathSpanCollector::new(db);
        path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

        let path_map = path_collector.path_map;

        let mut cursors = Vec::new();
        for (span, _) in path_map {
            let cursor = span.range.start();
            // println!("cursor from span: {:?}, {:?}", span, cursor);
            cursors.push(cursor);
        }

        cursors
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/single_ingot",
        glob: "**/lib.fe",
    )]
    fn test_goto_multiple_files(fixture: Fixture<&str>) {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir = Path::new(&cargo_manifest_dir).join("test_files/single_ingot");

        let db = &mut LanguageServerDatabase::default();
        let workspace = &mut Workspace::default();

        let _ = workspace.set_workspace_root(db, &ingot_base_dir);

        let fe_source_path = ingot_base_dir.join(fixture.path());
        let fe_source_path = fe_source_path.to_str().unwrap();
        let input = workspace.touch_input_for_file_path(db, fixture.path());
        assert_eq!(input.unwrap().ingot(db).kind(db), IngotKind::Local);

        input
            .unwrap()
            .set_text(db)
            .to((*fixture.content()).to_string());
        let top_mod = workspace
            .top_mod_from_file_path(db.as_lower_hir_db(), fe_source_path)
            .unwrap();

        let ingot = workspace.touch_ingot_for_file_path(db, fixture.path());
        assert_eq!(ingot.unwrap().kind(db), IngotKind::Local);

        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);
        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        for cursor in &cursors {
            let early_resolution = goto_enclosing_path(&db.snapshot(), top_mod, *cursor);

            let goto_info = match early_resolution {
                Some(EarlyResolvedPath::Full(bucket)) => {
                    if !bucket.is_empty() {
                        bucket
                            .iter()
                            .map(|x| x.pretty_path(db).unwrap())
                            .collect::<Vec<_>>()
                            .join("\n")
                    } else {
                        String::from("`NameResBucket` is empty")
                    }
                }
                Some(EarlyResolvedPath::Partial {
                    res,
                    unresolved_from: _,
                }) => res.pretty_path(db).unwrap(),
                None => String::from("No resolution available"),
            };

            cursor_path_map.insert(*cursor, goto_info);
        }

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_path_map
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {cursor:?}, path: {path:?}") })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "goto*.fe"
    )]
    fn test_goto_enclosing_path(fixture: Fixture<&str>) {
        let db = &mut LanguageServerDatabase::default();
        let workspace = &mut Workspace::default();
        let input = workspace
            .touch_input_for_file_path(db, fixture.path())
            .unwrap();
        input.set_text(db).to((*fixture.content()).to_string());
        let top_mod = workspace
            .top_mod_from_file_path(db.as_lower_hir_db(), fixture.path())
            .unwrap();

        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);

        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        for cursor in &cursors {
            let resolved_path = goto_enclosing_path(&db.snapshot(), top_mod, *cursor);

            if let Some(path) = resolved_path {
                match path {
                    EarlyResolvedPath::Full(bucket) => {
                        let path = bucket
                            .iter()
                            .map(|x| x.pretty_path(db).unwrap())
                            .collect::<Vec<_>>()
                            .join("\n");
                        cursor_path_map.insert(*cursor, path);
                    }
                    EarlyResolvedPath::Partial {
                        res,
                        unresolved_from: _,
                    } => {
                        let path = res.pretty_path(db).unwrap();
                        cursor_path_map.insert(*cursor, path);
                    }
                }
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
            let mut path_collector = PathSpanCollector::new(db);
            path_collector.visit_top_mod(&mut visitor_ctxt, top_mod);

            let path_map = path_collector.path_map;
            let enclosing_path = smallest_enclosing_path(*cursor, &path_map);

            let resolved_enclosing_path = hir_analysis::name_resolution::resolve_path_early(
                db,
                enclosing_path.unwrap().0,
                enclosing_path.unwrap().1,
            );

            let res = match resolved_enclosing_path {
                EarlyResolvedPath::Full(bucket) => bucket
                    .iter()
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
