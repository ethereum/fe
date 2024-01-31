mod test_db;
use std::path::Path;

use dir_test::{dir_test, Fixture};
use fe_compiler_test_utils::snap_test;
use fe_hir_analysis::ty::ty_check::check_func_body;
use test_db::HirAnalysisTestDb;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/ty_check",
    glob: "*.fe"
)]
fn test_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let (top_mod, mut prop_formatter) = db.new_stand_alone(file_name, fixture.content());

    for &func in top_mod.all_funcs(&db) {
        let Some(body) = func.body(&db) else {
            continue;
        };

        let typed_body = check_func_body(&db, func);
        for expr in body.exprs(&db).keys() {
            let ty = typed_body.expr_ty(&db, expr);
            prop_formatter.push_prop(
                func.top_mod(&db),
                expr.lazy_span(body).into(),
                ty.pretty_print(&db).to_string(),
            );
        }
    }

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}
