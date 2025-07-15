#[cfg(test)]
pub mod test_utils {
    use crate::backend::db::LanguageServerDatabase;
    use common::InputDb;
    use resolver::{Resolver, ingot::{IngotResolver, Ingot as ResolvedIngot, source_files::SourceFiles}};
    use url::Url;
    use std::path::Path;

    /// Load all files from an ingot directory into the database
    /// This is similar to what happens during initialization or when a new fe.toml is created
    pub fn load_ingot_from_directory(db: &mut LanguageServerDatabase, ingot_dir: &Path) {
        let mut ingot_resolver = IngotResolver::default();
        let ingot_url = Url::from_directory_path(ingot_dir)
            .expect("Failed to create URL from directory path");
        
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
            _ => panic!("Failed to resolve test ingot at {:?}", ingot_dir),
        }
    }
}