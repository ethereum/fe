use common::InputDb;
#[cfg(test)]
use driver::DriverDataBase;
use resolver::{
    ingot::{source_files::SourceFiles, Ingot as ResolvedIngot, IngotResolver},
    Resolver,
};
use std::path::Path;
use url::Url;

/// Load all files from an ingot directory into the database
/// This is similar to what happens during initialization or when a new fe.toml is created
#[cfg(test)]
pub fn load_ingot_from_directory(db: &mut DriverDataBase, ingot_dir: &Path) {
    let mut ingot_resolver = IngotResolver::default();
    let ingot_url =
        Url::from_directory_path(ingot_dir).expect("Failed to create URL from directory path");

    match ingot_resolver.resolve(&ingot_url) {
        Ok(ResolvedIngot::Folder {
            config,
            source_files: Some(SourceFiles { files, .. }),
        }) => {
            // Touch the config file if it exists
            if let Some(config) = config {
                db.workspace().touch(db, config.url, Some(config.content));
            }

            // Touch all source files
            for (file_url, content) in files {
                db.workspace().touch(db, file_url, Some(content));
            }
        }
        _ => panic!("Failed to resolve test ingot at {ingot_dir:?}"),
    }
}
