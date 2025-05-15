use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use test_utils::snap_test;
use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
    glob: "**/*.fe"
)]
fn run_ty_check(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.file_index().touch(
        &mut db,
        Url::from_file_path(fixture.path()).unwrap(),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_ty_check(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.file_index().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.run_on_top_mod(top_mod);
    }
}
