use camino::Utf8Path;
use common::ingot::{builtin_core, IngotBuilder};
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe"
)]
fn run_name_resolution(fixture: Fixture<&str>) {
    let db = DriverDataBase::default();
    let path = Utf8Path::new(fixture.path());

    let core = builtin_core(&db);
    let ingot = IngotBuilder::standalone(&db, path, fixture.content().to_string())
        .with_core_ingot(core)
        .build();
    let top_mod = db.top_mod_for_path(ingot, path).unwrap();

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe",
    postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_name_resolution(fixture: Fixture<&str>) {
        let db = DriverDataBase::default();
        let path = Utf8Path::new(fixture.path());

        let core = builtin_core(&db);
        let (ingot, file) = IngotBuilder::standalone(&db, path, fixture.content().to_string())
            .with_core_ingot(core)
            .build();
        let top_mod = db.top_mod(ingot, file);
        db.run_on_top_mod(top_mod);
    }
}
