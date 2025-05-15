use camino::Utf8PathBuf;
use radix_immutable::StringPrefixView;
use serde::Serialize;
use url::Url;

use crate::config::IngotMetadata;
// use crate::config::IngotManifest;
use crate::core::BUILTIN_CORE_BASE_URL;
use crate::file::{File, FileIndex};
use crate::urlext::UrlExt;
use crate::InputDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotKind {
    /// A standalone ingot is a dummy ingot when the compiler is invoked
    /// directly on a file.
    StandAlone,

    /// A local ingot which is the current ingot being compiled.
    Local,

    /// An external ingot which is depended on by the current ingot.
    External,

    /// Core library ingot.
    Core,
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub trait IngotBaseUrl {
    fn touch(
        &self,
        db: &mut dyn InputDb,
        path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> File;
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<IngotDescription<'db>>;
}

impl IngotBaseUrl for Url {
    fn touch(
        &self,
        db: &mut dyn InputDb,
        path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> File {
        let path = self
            .directory()
            .expect("failed to parse directory")
            .join(path.as_str())
            .expect("failed to parse path");
        db.file_index().touch(db, path, initial_content)
    }
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<IngotDescription<'db>> {
        db.file_index().containing_ingot(db, self)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct IngotDescription<'db> {
    pub base: Url,
    pub standalone_file: Option<File>,
    pub index: FileIndex,
    pub version: Version,
    pub kind: IngotKind,
    pub dependencies: Vec<(String, Url)>,
}

#[derive(Debug)]
pub enum IngotError {
    RootFileNotFound,
}

impl IngotDescription<'_> {
    pub fn root_file(&self, db: &dyn InputDb) -> Result<File, IngotError> {
        if let Some(root_file) = self.standalone_file(db) {
            Ok(root_file)
        } else {
            let path = self
                .base(db)
                .join("src/lib.fe")
                .expect("failed to join path");
            self.index(db)
                .get(db, &path)
                .ok_or(IngotError::RootFileNotFound)
        }
    }

    pub fn files(&self, db: &dyn InputDb) -> StringPrefixView<Url, File> {
        if let Some(standalone_file) = self.standalone_file(db) {
            // For standalone ingots, use the standalone file URL as the base
            self.index(db).items_at_base(
                db,
                standalone_file
                    .url(db)
                    .expect("file should be registered in the index"),
            )
        } else {
            // For regular ingots, use the ingot base URL
            self.index(db).items_at_base(db, self.base(db))
        }
    }
}

pub trait IngotIndex {
    fn containing_ingot_base(&self, db: &dyn InputDb, location: &Url) -> Option<Url>;
    fn containing_ingot_config(self, db: &dyn InputDb, location: Url) -> Option<File>;
    fn containing_ingot<'db>(
        self,
        db: &'db dyn InputDb,
        location: &Url,
    ) -> Option<IngotDescription<'db>>;
    fn touch_ingot<'db>(
        self,
        db: &'db mut dyn InputDb,
        base_url: &Url,
        initial_config: IngotMetadata,
    ) -> Option<IngotDescription<'db>>;
}

pub type Version = serde_semver::semver::Version;
#[salsa::tracked]
impl IngotIndex for FileIndex {
    fn containing_ingot_base(&self, db: &dyn InputDb, location: &Url) -> Option<Url> {
        self.containing_ingot_config(db, location.clone())
            .map(move |config| {
                let dir = config
                    .url(db)
                    .expect("Config file should be indexed")
                    .directory()
                    .expect("Config URL should have a directory");
                dir
            })
    }
    /// Recursively search for a local ingot configuration file
    #[salsa::tracked]
    fn containing_ingot_config(self, db: &dyn InputDb, file: Url) -> Option<File> {
        tracing::debug!(target: "ingot_config", "containing_ingot_config called with file: {}", file);
        let dir = match file.directory() {
            Some(d) => d,
            None => {
                tracing::debug!(target: "ingot_config", "Could not get directory for: {}", file);
                return None;
            }
        };
        tracing::debug!(target: "ingot_config", "Search directory: {}", dir);

        let config_url = match dir.join("fe.toml") {
            Ok(url) => url,
            Err(_) => {
                tracing::debug!(target: "ingot_config", "Could not join 'fe.toml' to dir: {}", dir);
                return None;
            }
        };
        tracing::debug!(target: "ingot_config", "Looking for config file at: {}", config_url);

        if let Some(file_obj) = self.get(db, &config_url) {
            tracing::debug!(target: "ingot_config", "Found config file in index: {}", config_url);
            return Some(file_obj);
        } else {
            tracing::debug!(target: "ingot_config", "Config file NOT found in index: {}. Checking parent.", config_url);
            if let Some(parent_dir_url) = dir.parent() {
                tracing::debug!(target: "ingot_config", "Recursively calling containing_ingot_config for parent: {}", parent_dir_url);
                self.containing_ingot_config(db, parent_dir_url)
            } else {
                tracing::debug!(target: "ingot_config", "No parent directory for {}, stopping search.", dir);
                None
            }
        }
    }

    fn containing_ingot<'db>(
        self,
        db: &'db dyn InputDb,
        location: &Url,
    ) -> Option<IngotDescription<'db>> {
        containing_ingot_impl(db, self, location.clone())
    }

    fn touch_ingot<'db>(
        self,
        db: &'db mut dyn InputDb,
        base_url: &Url,
        config: IngotMetadata,
    ) -> Option<IngotDescription<'db>> {
        let base_dir = base_url
            .directory()
            .expect("Base URL should have a directory");
        let config_file = base_dir
            .join("fe.toml")
            .expect("Config file should be indexed");
        // Wrap the config in a proper IngotConfig wrapper for serialization
        let ingot_config = IngotConfig { ingot: config };
        let config_toml = toml::to_string(&ingot_config).unwrap_or_default();
        let config = self.touch(db, config_file, Some(config_toml));

        config.containing_ingot(db)
    }
}

/// A wrapper struct to ensure the config is serialized with an [ingot] table
#[derive(Serialize)]
struct IngotConfig {
    ingot: IngotMetadata,
}

#[salsa::tracked]
fn containing_ingot_impl<'db>(
    db: &'db dyn InputDb,
    index: FileIndex,
    location: Url,
) -> Option<IngotDescription<'db>> {
    let core_url = Url::parse(BUILTIN_CORE_BASE_URL).expect("Failed to parse core URL");
    let is_core = location.scheme().contains("core");
    let dependencies = if is_core {
        vec![]
    } else {
        vec![("core".into(), core_url)]
    };

    match index.containing_ingot_base(db, &location) {
        Some(ingot_url) => Some(IngotDescription::new(
            db,
            ingot_url,
            None,
            index,
            Version::new(1, 0, 0),
            if is_core {
                IngotKind::Core
            } else {
                IngotKind::Local
            },
            dependencies,
        )),
        None => {
            // Make a standalone ingot if no base is found
            let base = location.directory().unwrap_or_else(|| location.clone());
            let specific_root_file = if location.path().ends_with(".fe") {
                index.get(db, &location)
            } else {
                None
            };
            Some(IngotDescription::new(
                db,
                base,
                specific_root_file,
                index,
                Version::new(0, 0, 0),
                IngotKind::StandAlone,
                dependencies,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::file::File;

    use super::*;

    use crate::define_input_db;
    
    define_input_db!(TestDatabase);
    #[test]
    fn test_locate_config() {
        let mut db = TestDatabase::default();
        let index = db.file_index();

        // Create our test files - a library file, a config file, and a standalone file
        let url_lib = Url::parse("file:///foo/src/lib.fe").unwrap();
        let lib = File::__new_impl(&db, "lib".to_string());

        let url_config = Url::parse("file:///foo/fe.toml").unwrap();
        let config = File::__new_impl(&db, "config".to_string());

        let url_standalone = Url::parse("file:///bar/standalone.fe").unwrap();
        let standalone = File::__new_impl(&db, "standalone".to_string());

        // Add the files to the index
        index
            .set(&mut db, url_lib.clone(), lib)
            .expect("Failed to set lib file");
        index
            .set(&mut db, url_config.clone(), config)
            .expect("Failed to set config file");
        index
            .set(&mut db, url_standalone.clone(), standalone)
            .expect("Failed to set standalone file");

        // Test recursive search: lib.fe is in /foo/src/ but config is in /foo/
        // This tests that we correctly search up the directory tree
        let found_config = index.containing_ingot_config(&db, url_lib);
        assert!(found_config.is_some());
        assert_eq!(found_config.and_then(|c| c.url(&db)).unwrap(), url_config);

        // Test that standalone file without a config returns None
        let no_config = index.containing_ingot_config(&db, url_standalone);
        assert!(no_config.is_none());
    }
}
