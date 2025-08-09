use camino::Utf8PathBuf;
use common::{urlext::canonical_url, InputDb};
use driver::DriverDataBase;
use url::Url;

pub fn check(path: &Utf8PathBuf) {
    let mut db = DriverDataBase::default();
    let ingot_url = canonical_url(path);
    let init_diagnostics = driver::init_ingot(&mut db, &ingot_url);

    // Print workspace setup diagnostics if any
    if !init_diagnostics.is_empty() {
        for diagnostic in &init_diagnostics {
            eprintln!("{diagnostic}");
        }
    }

    let ingot = db.workspace().containing_ingot(&db, &ingot_url).unwrap();
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(&db);
    }

    // Collect all dependencies with errors
    let mut dependency_errors = Vec::new();
    for dependency_url in db.graph().dependency_urls(&db, &ingot_url) {
        let ingot = db
            .workspace()
            .containing_ingot(&db, &dependency_url)
            .unwrap();
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }

    // Print dependency errors if any exist
    if !dependency_errors.is_empty() {
        if dependency_errors.len() == 1 {
            eprintln!("❌ Error in dependency ingot");
        } else {
            eprintln!("❌ Errors in dependency ingots");
        }

        for (dependency_url, diags) in dependency_errors {
            print_dependency_info(&db, &dependency_url);
            diags.emit(&db);
        }
    }
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    // Get the ingot for this dependency URL to access its config
    if let Some(ingot) = db.workspace().containing_ingot(db, dependency_url) {
        if let Some(config) = ingot.config(db) {
            let name = config.metadata.name.as_deref().unwrap_or("unknown");
            if let Some(version) = &config.metadata.version {
                eprintln!("{name} (version: {version})");
            } else {
                eprintln!("{name}");
            }
        } else {
            eprintln!("Unknown dependency");
        }
    } else {
        eprintln!("Unknown dependency");
    }

    eprintln!("{dependency_url}");
}
