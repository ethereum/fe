use common::indexmap::IndexMap;
use parser::TextRange;

use crate::{
    collect::collect_item_paths,
    hir_def::{item::ItemKind, GenericParamOwner, PathId},
    span::{self, LazySpan},
    SpannedHirDb,
};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct DeclSourceMap<'db> {
    /// Map header path spans (text ranges) to (PathId, LazyPathSpan)
    pub path_entries: IndexMap<TextRange, (PathId<'db>, span::path::LazyPathSpan<'db>)>,
    /// Map generic parameter name spans (text ranges) to generic param index
    pub generic_param_name_entries: IndexMap<TextRange, u16>,
}

impl<'db> DeclSourceMap<'db> {
    fn insert_path_entry(&mut self, range: TextRange, id: PathId<'db>, span: span::path::LazyPathSpan<'db>) {
        self.path_entries.insert(range, (id, span));
    }

    // Future: add more entries (types, generics) as needed.
}

pub fn decl_source_map<'db>(db: &'db dyn SpannedHirDb, item: ItemKind<'db>) -> DeclSourceMap<'db> {
    let mut map = DeclSourceMap::default();
    // Collect body ranges for nested bodies to exclude them from declaration map
    let mut body_ranges: Vec<TextRange> = Vec::new();
    {
        use crate::hir_def::ItemKind as IK;
        let s_graph = item.top_mod(db).scope_graph(db);
        let mut stack: Vec<IK<'db>> = vec![item];
        while let Some(it) = stack.pop() {
            // Push children for recursive traversal
            for child in s_graph.child_items(it.scope()) {
                stack.push(child);
            }
            match it {
                IK::Func(f) => {
                    if let Some(body) = f.body(db) {
                        if let Some(sp) = body.span().resolve(db) {
                            body_ranges.push(sp.range);
                        }
                    }
                }
                IK::Const(c) => {
                    if let Some(body) = c.body(db).to_opt() {
                        if let Some(sp) = body.span().resolve(db) {
                            body_ranges.push(sp.range);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Insert only header/signature paths (exclude those inside any body range)
    let collected = collect_item_paths(db, item).clone();
    'entries: for (path_id, pspan) in collected {
        if let Some(sp) = pspan.resolve(db) {
            for br in &body_ranges {
                if br.contains(sp.range.start()) && br.contains(sp.range.end()) {
                    // Skip body-contained paths
                    continue 'entries;
                }
            }
            map.insert_path_entry(sp.range, path_id, pspan);
        }
    }

    // Collect generic parameter name spans for this item's header (if any)
    if let Some(owner) = GenericParamOwner::from_item_opt(item) {
        let gp_span = owner.params_span();
        let data = owner.params(db).data(db).clone();
        for (idx, gparam) in data.into_iter().enumerate() {
            use crate::hir_def::GenericParam;
            match gparam {
                GenericParam::Type(_) => {
                    let name_span = gp_span.clone()
                        .param(idx)
                        .into_type_param()
                        .name()
                        .resolve(db);
                    if let Some(ns) = name_span {
                        map.generic_param_name_entries.insert(ns.range, idx as u16);
                    }
                }
                GenericParam::Const(_) => {
                    let name_span = gp_span.clone()
                        .param(idx)
                        .into_const_param()
                        .name()
                        .resolve(db);
                    if let Some(ns) = name_span {
                        map.generic_param_name_entries.insert(ns.range, idx as u16);
                    }
                }
            }
        }
    }
    map
}
