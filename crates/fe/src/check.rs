use camino::Utf8PathBuf;
use driver::DriverDataBase;
use hir::Workspace;
use url::Url;

pub fn _check(path: &Utf8PathBuf) -> (DriverDataBase, Workspace) {
    let mut db = DriverDataBase::default();
    let workspace = driver::setup_workspace(&mut db, path);
    (db, workspace)
}

pub fn check(path: &Utf8PathBuf) {
    let (db, workspace) = _check(path);
    let ingot = workspace
        .containing_ingot(
            &db,
            &Url::from_file_path(path.canonicalize_utf8().unwrap().join("fe.toml")).unwrap(),
        )
        .unwrap();
    let diags = db.run_on_ingot(ingot);
    diags.emit(&db);
}
