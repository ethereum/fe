mod test_db;

use camino::Utf8Path;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;

#[test]
fn analyze_corelib() {
    let mut db = DriverDataBase::default();
    let (core, _) = db.static_core_ingot();

    let core_diags = db.run_on_ingot(core);
    if !(core_diags.is_empty()) {
        core_diags.emit(&db);
        panic!("expected no diagnostics");
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/corelib",
    glob: "*.fe"
)]
fn corelib_standalone(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let path = Utf8Path::new(fixture.path());
    let file_name = path.file_name().unwrap();
    let (core, _) = db.static_core_ingot();
    let (ingot, _) = db.standalone(file_name.into(), fixture.content(), core);

    let local_diags = db.run_on_ingot(ingot);
    if !local_diags.is_empty() {
        local_diags.emit(&db);
        panic!("expected no diagnostics");
    }
}
