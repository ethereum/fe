use std::path::Path;

use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_compiler_test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
    glob: "**/*.fe"
)]
fn run_ty_check(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Path::new(fixture.path());

    let input_file = db.standalone(path, fixture.content());
    let top_mod = db.top_mod(input_file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;

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
        let path = Path::new(fixture.path());

        let input_file = db.standalone(path, fixture.content());
        let top_mod = db.top_mod(input_file);
        db.run_on_top_mod(top_mod);
    }
}
