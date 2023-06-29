use std::path::Path;

use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_compiler_test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe"
)]
fn run_name_resolution(fixture: Fixture<&str>) {
    let mut driver = DriverDataBase::default();
    let path = Path::new(fixture.path());
    driver.run_on_file(path, fixture.content());
    let diags = driver.format_diags();
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
        let mut driver = DriverDataBase::default();
        let path = Path::new(fixture.path());
        driver.run_on_file(path, fixture.content());
    }
}
