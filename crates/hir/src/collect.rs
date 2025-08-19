use crate::{
    hir_def::{path::PathId, ItemKind},
    span::path::LazyPathSpan,
    visitor::{Visitor, VisitorCtxt},
    HirDb,
};

/// Lightweight path span collector for a single item
#[derive(Default)]
pub struct PathSpanCollector<'db> {
    pub paths: Vec<(PathId<'db>, LazyPathSpan<'db>)>,
}

impl<'db, 'ast: 'db> Visitor<'ast> for PathSpanCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, LazyPathSpan<'ast>>, path: PathId<'db>) {
        let Some(span) = ctxt.span() else { return; };
        self.paths.push((path, span));
    }
}

/// Cached collection of all HIR paths and their lazy spans within an item
pub fn collect_item_paths<'db>(
    db: &'db dyn HirDb,
    item: ItemKind<'db>,
) -> Vec<(PathId<'db>, LazyPathSpan<'db>)> {
    let mut vctxt = VisitorCtxt::with_item(db, item);
    let mut collector = PathSpanCollector::default();
    collector.visit_item(&mut vctxt, item);
    collector.paths
}

use crate::{span::{DynLazySpan, LazySpan}, SpannedHirDb};
use parser::{TextRange, TextSize};

/// Find the innermost item that contains the given cursor position.
pub fn find_enclosing_item_at<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: crate::hir_def::TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<ItemKind<'db>> {
    let mut best: Option<(ItemKind<'db>, TextRange)> = None;
    for item in top_mod.scope_graph(db).items_dfs(db) {
        if let Some(span) = DynLazySpan::from(item.span()).resolve(db) {
            if span.range.contains(cursor) {
                let len = span.range.end() - span.range.start();
                if best
                    .as_ref()
                    .map(|(_, r)| (r.end() - r.start()) > len)
                    .unwrap_or(true)
                {
                    best = Some((item, span.range));
                }
            }
        }
    }
    best.map(|(it, _)| it)
}

use crate::synthesis::LazyHirResult;
use crate::hir_def::{Body, TopLevelMod};

/// From a lazy HIR lookup result, find the nearest containing item.
pub fn enclosing_item_from_lazy_result<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    res: &LazyHirResult<'db>,
) -> Option<ItemKind<'db>> {
    match res {
        LazyHirResult::Expr(body, _, _) | LazyHirResult::Stmt(body, _, _) | LazyHirResult::Pat(body, _) => {
            let scope = body.scope();
            scope.parent_item(db)
        }
        LazyHirResult::ItemPath(item, _, _, _)
        | LazyHirResult::ItemType(item, _, _)
        | LazyHirResult::ItemGenericParam(item, _) => Some(*item),
        LazyHirResult::None => find_enclosing_item_at(db, top_mod, TextSize::from(0u32)),
    }
}
