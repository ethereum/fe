use fxhash::FxHashMap;
use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind, PathId, TopLevelMod},
    visitor::{prelude::LazyPathSpan, Visitor, VisitorCtxt},
    HirDb,
};
use hir_analysis::name_resolution::EarlyResolvedPath;

use crate::db::{LanguageServerDatabase, LanguageServerDb};
use common::diagnostics::Span;
use hir::span::LazySpan;

pub(crate) type GotoEnclosingPath = (PathId, ScopeId);
pub(crate) type GotoPathMap = FxHashMap<Span, GotoEnclosingPath>;

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

pub(crate) type Cursor = rowan::TextSize;

impl<'db> Visitor for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'_, LazyPathSpan>, path: PathId) {
        let Some(span) = ctxt
            .span()
            .map(|lazy_span| lazy_span.resolve(self.db.as_spanned_hir_db()))
            .flatten()
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

    return smallest_enclosing_path;
}

pub fn goto_enclosing_path(
    db: &mut LanguageServerDatabase,
    top_mod: TopLevelMod,
    cursor: Cursor,
) -> Option<EarlyResolvedPath> {
    // Find the innermost item enclosing the cursor.
    let item: ItemKind = db.find_enclosing_item(top_mod, cursor)?;

    let mut visitor_ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    let mut path_collector = PathSpanCollector::new(&db);
    path_collector.visit_item(&mut visitor_ctxt, item);

    let path_map = path_collector.path_map;

    // Find the path that encloses the cursor.
    let goto_starting_path = smallest_enclosing_path(cursor, &path_map)?;

    let (path_id, scope_id) = goto_starting_path;

    // Resolve path.
    let resolved_path = hir_analysis::name_resolution::resolve_path_early(db, path_id, scope_id);

    Some(resolved_path)
}

#[cfg(test)]
mod tests {
    use crate::workspace::{IngotFileContext, Workspace};

    use super::*;
    use common::input::IngotKind;
    use dir_test::{dir_test, Fixture};
    use fe_compiler_test_utils::snap_test;
    use std::path::Path;

    fn extract_multiple_cursor_positions_from_spans(
        db: &mut LanguageServerDatabase,
        top_mod: TopLevelMod,
    ) -> Vec<rowan::TextSize> {
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
        let mut path_collector = PathSpanCollector::new(&db);
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

        let _ = workspace.set_workspace_root(db, &Some(ingot_base_dir.clone()));

        let fe_source_path = ingot_base_dir.join(fixture.path());
        let input = workspace.input_from_file_path(db, fixture.path());
        assert_eq!(input.unwrap().ingot(db).kind(db), IngotKind::Local);

        let top_mod = workspace.top_mod_from_file(db, &fe_source_path, fixture.content()).unwrap();

        let ingot = workspace.ingot_from_file_path(db, fixture.path());
        assert_eq!(ingot.unwrap().kind(db), IngotKind::Local);

        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);
        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        cursors.iter().for_each(|cursor| {
            let early_resolution = goto_enclosing_path(db, top_mod, *cursor);

            let goto_info = match early_resolution {
                Some(EarlyResolvedPath::Full(bucket)) => {
                    if bucket.len() > 0 {
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
        });

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_path_map
                .iter()
                .map(|(cursor, path)| {
                    format!("cursor position: {:?}, path: {:?}", cursor, path)
                })
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
        let mut db = &mut LanguageServerDatabase::default();
        let workspace = &mut Workspace::default();
        let path = Path::new(fixture.path());
        let top_mod = workspace.top_mod_from_file(&mut db, path, fixture.content()).unwrap();

        let cursors = extract_multiple_cursor_positions_from_spans(&mut db, top_mod);

        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        cursors.iter().for_each(|cursor| {
            let resolved_path = goto_enclosing_path(db, top_mod, *cursor);

            match resolved_path {
                Some(path) => match path {
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
                },
                None => {}
            };
        });

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_path_map
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {:?}, path: {}", cursor, path) })
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
        let path = Path::new(fixture.path());
        let top_mod = workspace.top_mod_from_file(db, path, fixture.content()).unwrap();

        let cursors = extract_multiple_cursor_positions_from_spans(db, top_mod);

        let mut cursor_path_map: FxHashMap<Cursor, String> = FxHashMap::default();

        cursors.iter().for_each(|cursor| {
            let mut visitor_ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
            let mut path_collector = PathSpanCollector::new(&db);
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
        });

        let result = format!(
            "{}\n---\n{}",
            fixture.content(),
            cursor_path_map
                .iter()
                .map(|(cursor, path)| { format!("cursor position: {:?}, path: {}", cursor, path) })
                .collect::<Vec<_>>()
                .join("\n")
        );
        snap_test!(result, fixture.path());
    }
}
