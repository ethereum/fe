use camino::Utf8PathBuf;
use common::urlext::canonical_url;
use url::Url;

pub fn check(path: &Utf8PathBuf) {
    let (db, workspace, base_ingot_url, dependency_ingot_urls, diagnostics) =
        driver::setup_workspace(path);

    for diagnostic in diagnostics {
        println!("{:?}", diagnostic);
    }

    let ingot = workspace.containing_ingot(&db, &base_ingot_url).unwrap();
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(&db);
    }

    for dependency_url in dependency_ingot_urls {
        println!("{}", dependency_url);
        let ingot = workspace.containing_ingot(&db, &dependency_url).unwrap();
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            diags.emit(&db);
        }
    }
}
