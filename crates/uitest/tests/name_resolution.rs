use camino::Utf8Path;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe"
)]
fn run_name_resolution(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Utf8Path::new(fixture.path());

    let (core, _) = db.static_core_ingot();
    let (ingot, file) = db.standalone(path, fixture.content(), core);
    let top_mod = db.top_mod(ingot, file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
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
    fn run_name_resolution(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let path = Utf8Path::new(fixture.path());

        let (core, _) = db.static_core_ingot();
        let (ingot, file) = db.standalone(path, fixture.content(), core);
        let top_mod = db.top_mod(ingot, file);
        db.run_on_top_mod(top_mod);
    }
}
