use std::path::Path;

use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_compiler_test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/def",
    glob: "*.fe"
)]
fn run_ty_def(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Path::new(fixture.path());

    let (ingot, file) = db.standalone(path, fixture.content());
    let top_mod = db.top_mod(ingot, file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/const_ty",
    glob: "*.fe"
)]
fn run_const_ty(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Path::new(fixture.path());

    let (ingot, file) = db.standalone(path, fixture.content());
    let top_mod = db.top_mod(ingot, file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_bound",
    glob: "*.fe"
)]
fn run_trait_bound(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Path::new(fixture.path());

    let (ingot, file) = db.standalone(path, fixture.content());
    let top_mod = db.top_mod(ingot, file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_impl",
    glob: "*.fe"
)]
fn run_trait_impl(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Path::new(fixture.path());

    let (ingot, file) = db.standalone(path, fixture.content());
    let top_mod = db.top_mod(ingot, file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;

    mod def {
        use super::*;

        // TODO: we opt out the tests for type/alias/trait-infinite recursion checks. See https://github.com/ethereum/fe/issues/939 for more details.
        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/def",
        glob: "*[!_cycle].fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_ty_def(fixture: Fixture<&str>) {
            let mut db = DriverDataBase::default();
            let path = Path::new(fixture.path());

            let (ingot, file) = db.standalone(path, fixture.content());
            let top_mod = db.top_mod(ingot, file);
            db.run_on_top_mod(top_mod);
        }
    }

    mod const_ty {
        use super::*;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/const_ty",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_const_ty(fixture: Fixture<&str>) {
            let mut db = DriverDataBase::default();
            let path = Path::new(fixture.path());

            let (ingot, file) = db.standalone(path, fixture.content());
            let top_mod = db.top_mod(ingot, file);
            db.run_on_top_mod(top_mod);
        }
    }

    mod trait_bound {
        use super::*;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_bound",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_trait_bound(fixture: Fixture<&str>) {
            let mut db = DriverDataBase::default();
            let path = Path::new(fixture.path());

            let (ingot, file) = db.standalone(path, fixture.content());
            let top_mod = db.top_mod(ingot, file);
            db.run_on_top_mod(top_mod);
        }
    }

    mod trait_impl {
        use super::*;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_impl",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_trait_impl(fixture: Fixture<&str>) {
            let mut db = DriverDataBase::default();
            let path = Path::new(fixture.path());

            let (ingot, file) = db.standalone(path, fixture.content());
            let top_mod = db.top_mod(ingot, file);
            db.run_on_top_mod(top_mod);
        }
    }
}
