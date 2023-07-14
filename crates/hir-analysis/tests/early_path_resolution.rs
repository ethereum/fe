mod test_db;
use test_db::{HirAnalysisTestDb, HirPropertyFormatter};

use std::path::Path;

use dir_test::{dir_test, Fixture};
use fe_compiler_test_utils::snap_test;
use fe_hir_analysis::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, PathAnalysisPass},
    HirAnalysisDb,
};
use hir::{
    analysis_pass::ModuleAnalysisPass,
    hir_def::{Expr, ExprId, ItemKind, Pat, PatId, PathId, TopLevelMod, TypeId},
    visitor::prelude::*,
    HirDb, SpannedHirDb,
};

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/early_path_resolution",
    glob: "*.fe"
)]
fn test_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let (top_mod, mut prop_formatter) = db.new_stand_alone(file_name, fixture.content());

    let mut pass = PathAnalysisPass::new(&db);
    let diags = pass.run_on_module(top_mod);
    if !diags.is_empty() {
        for diag in diags {
            println!("{}", diag.to_complete(db.as_spanned_hir_db()).message);
        }
        panic!("Failed to resolve paths");
    }

    let mut ctxt = VisitorCtxt::with_top_mod(db.as_hir_db(), top_mod);
    PathVisitor {
        db: &db,
        top_mod,
        domain_stack: Vec::new(),
        prop_formatter: &mut prop_formatter,
    }
    .visit_top_mod(&mut ctxt, top_mod);

    let res = prop_formatter.finish(db.as_spanned_hir_db());
    snap_test!(res, fixture.path());
}

struct PathVisitor<'db, 'a> {
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod,
    domain_stack: Vec<NameDomain>,
    prop_formatter: &'a mut HirPropertyFormatter,
}

impl<'db, 'a> Visitor for PathVisitor<'db, 'a> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'_, LazyItemSpan>, item: ItemKind) {
        if matches!(item, ItemKind::Use(_)) {
            return;
        }

        self.domain_stack.push(NameDomain::Type);
        walk_item(self, ctxt, item);
        self.domain_stack.pop();
    }

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, ty: TypeId) {
        self.domain_stack.push(NameDomain::Type);
        walk_ty(self, ctxt, ty);
        self.domain_stack.pop();
    }

    fn visit_pat(&mut self, _: &mut VisitorCtxt<'_, LazyPatSpan>, _: PatId, _: &Pat) {}

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyExprSpan>,
        expr: ExprId,
        expr_data: &Expr,
    ) {
        if matches!(expr_data, Expr::Block { .. }) {
            walk_expr(self, ctxt, expr);
        }
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'_, LazyPathSpan>, path: PathId) {
        let scope = ctxt.scope();
        let resolved_path = resolve_path_early(self.db.as_hir_analysis_db(), path, scope);
        match resolved_path {
            EarlyResolvedPath::Full(bucket) => {
                let domain = self.domain_stack.last().copied().unwrap();
                let res = bucket.pick(domain).as_ref().unwrap();
                let prop = res.pretty_path(self.db.as_hir_analysis_db()).unwrap();
                let span = ctxt
                    .span()
                    .unwrap()
                    .segment(path.len(self.db.as_hir_db()) - 1)
                    .into();
                self.prop_formatter.push_prop(self.top_mod, span, prop);
            }

            EarlyResolvedPath::Partial {
                res,
                unresolved_from,
            } => {
                let prop = res.pretty_path(self.db.as_hir_analysis_db()).unwrap();
                let span = ctxt.span().unwrap().segment(unresolved_from - 1).into();
                self.prop_formatter.push_prop(self.top_mod, span, prop);
            }
        }
    }
}
