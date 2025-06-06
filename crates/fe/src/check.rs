use camino::Utf8PathBuf;
use url::Url;

pub fn check(path: &Utf8PathBuf) {
    let (db, workspace, diagnostics) = driver::setup_workspace(path);

    for diagnostic in diagnostics {
        println!("{:?}", diagnostic);
    }

    let ingot = workspace
        .containing_ingot(
            &db,
            &Url::from_file_path(path.canonicalize_utf8().unwrap().join("fe.toml")).unwrap(),
            // &Url::from_directory_path(path.canonicalize_utf8().unwrap())
            //     .unwrap()
            //     .directory()
            //     .unwrap(),
        )
        .unwrap();
    let diags = db.run_on_ingot(ingot);
    diags.emit(&db);
}
