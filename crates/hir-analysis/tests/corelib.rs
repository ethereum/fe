mod test_db;

use camino::Utf8Path;
use common::ingot::{builtin_core, IngotBuilder};
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;

#[test]
fn analyze_corelib() {
    let db = DriverDataBase::default();
    let core = builtin_core(&db);

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
    let db = DriverDataBase::default();
    let path = Utf8Path::new(fixture.path());
    let core = builtin_core(&db);
    let (ingot, _) = IngotBuilder::standalone(&db, path, fixture.content().to_string())
        .with_core_ingot(core)
        .build();

    let local_diags = db.run_on_ingot(ingot);
    if !local_diags.is_empty() {
        local_diags.emit(&db);
        panic!("expected no diagnostics");
    }
}
