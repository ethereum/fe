use camino::Utf8PathBuf;
use common::urlext::canonical_url;
use common::InputDb;
use driver::DriverDataBase;

pub fn check(path: &Utf8PathBuf) {
    let mut db = DriverDataBase::default();
    let ingot_url = canonical_url(path).unwrap();
    driver::init_workspace_ingot(&mut db, &ingot_url);

    let ingot = db.workspace().containing_ingot(&db, &ingot_url).unwrap();
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(&db);
    }
}
