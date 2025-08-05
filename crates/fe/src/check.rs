use camino::Utf8PathBuf;
use common::{urlext::canonical_url, InputDb};
use driver::DriverDataBase;

pub fn check(path: &Utf8PathBuf) {
    let mut db = DriverDataBase::default();
    let ingot_url = canonical_url(path);
    let workspace_diagnostics = driver::init_workspace_ingot(&mut db, &ingot_url);

    // Print workspace setup diagnostics if any
    if !workspace_diagnostics.is_empty() {
        for diagnostic in &workspace_diagnostics {
            eprintln!("{diagnostic}");
        }
    }

    let ingot = db.workspace().containing_ingot(&db, &ingot_url).unwrap();
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        eprintln!("errors in {ingot_url}");
        diags.emit(&db);
    }

    for dependency_url in db.graph().dependency_urls(&db, &ingot_url) {
        let ingot = db
            .workspace()
            .containing_ingot(&db, &dependency_url)
            .unwrap();
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            eprintln!("errors in {dependency_url}");
            diags.emit(&db);
        }
    }
}
