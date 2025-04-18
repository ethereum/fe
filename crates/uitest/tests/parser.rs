use camino::Utf8Path;
use common::ingot::IngotBuilder;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use hir_analysis::analysis_pass::{AnalysisPassManager, ParsingPass};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/parser",
    glob: "*.fe"
)]
fn run_parser(fixture: Fixture<&str>) {
    let db = DriverDataBase::default();
    let path = Utf8Path::new(fixture.path());

    let ingot = IngotBuilder::standalone(&db, path, fixture.content().to_string()).build();
    let top_mod = db.top_mod_for_path(ingot, path).unwrap();

    let diags = db.run_on_file_with_pass_manager(top_mod, init_parser_pass);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

fn init_parser_pass(db: &DriverDataBase) -> AnalysisPassManager<'_> {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    pass_manager
}

#[cfg(target_family = "wasm")]
mod wasm {
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_parser(fixture: Fixture<&str>) {
        let db = DriverDataBase::default();
        let path = Utf8Path::new(fixture.path());

        let (ingot, file) =
            IngotBuilder::standalone(&db, path, fixture.content().to_string()).build();
        let top_mod = db.top_mod(ingot, file);
        db.run_on_top_mod(top_mod);
    }
}
