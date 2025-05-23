mod test_db;

use common::core::HasBuiltinCore;
use common::ingot::IngotIndex;
use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use test_utils::url_utils::UrlExt;
use url::Url;

#[test]
fn analyze_corelib() {
    let db = DriverDataBase::default();
    let core = db.builtin_core();

    let core_diags = db.run_on_ingot(core);
    if !(core_diags.is_empty()) {
        core_diags.emit(&db);
        panic!(
            "expected no diagnostics, but got:\n{:?}",
            core_diags.format_diags(&db)
        );
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/corelib",
    glob: "*.fe"
)]
fn corelib_standalone(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Url::from_file_path(&path.canonicalize_utf8().unwrap()).unwrap();
    db.workspace()
        .touch(&mut db, path.clone(), Some(fixture.content().to_string()));

    let local_diags = db.run_on_ingot(
        db.workspace()
            .containing_ingot(&db, &path)
            .expect("Failed to find containing ingot"),
    );
    if !local_diags.is_empty() {
        local_diags.emit(&db);
        panic!("expected no diagnostics");
    }
}
