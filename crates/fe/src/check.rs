use camino::Utf8PathBuf;
use common::urlext::canonical_url;
use url::Url;

pub fn check(path: &Utf8PathBuf) {
    let (db, workspace, diagnostics) = driver::setup_workspace(path);

    for diagnostic in diagnostics {
        println!("{:?}", diagnostic);
    }

    let ingot = workspace
        // should probably get url from setup
        .containing_ingot(&db, &canonical_url(path).unwrap())
        .unwrap();
    let diags = db.run_on_ingot(ingot);
    diags.emit(&db);
}
