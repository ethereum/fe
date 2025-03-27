use core::panic;

use camino::Utf8PathBuf;
use radix_immutable::StringPrefixView;
use smol_str::SmolStr;
use url::Url;

use crate::config::{Config, Dependency, DependencyDescription, IngotMetadata};
use crate::core::BUILTIN_CORE_BASE_URL;
use crate::file::{File, Workspace};
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

pub trait IngotBaseUrl {
    fn touch(
        &self,
        db: &mut dyn InputDb,
        path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> File;
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<Ingot<'db>>;
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
        db.workspace().touch(db, path, initial_content)
    }
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<Ingot<'db>> {
        db.workspace().containing_ingot(db, self)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct Ingot<'db> {
    pub base: Url,
    pub standalone_file: Option<File>,
    pub kind: IngotKind,
}

#[derive(Debug)]
pub enum IngotError {
    RootFileNotFound,
}

#[salsa::tracked]
impl<'db> Ingot<'db> {
    pub fn root_file(&self, db: &dyn InputDb) -> Result<File, IngotError> {
        if let Some(root_file) = self.standalone_file(db) {
            Ok(root_file)
        } else {
            let path = self
                .base(db)
                .join("src/lib.fe")
                .expect("failed to join path");
            db.workspace()
                .get(db, &path)
                .ok_or(IngotError::RootFileNotFound)
        }
    }

    pub fn files(&self, db: &dyn InputDb) -> StringPrefixView<Url, File> {
        if let Some(standalone_file) = self.standalone_file(db) {
            // For standalone ingots, use the standalone file URL as the base
            db.workspace().items_at_base(
                db,
                standalone_file
                    .url(db)
                    .expect("file should be registered in the index"),
            )
        } else {
            // For regular ingots, use the ingot base URL
            db.workspace().items_at_base(db, self.base(db))
        }
    }

    #[salsa::tracked]
    pub fn config(self, db: &'db dyn InputDb) -> Option<Config> {
        db.workspace()
            .containing_ingot_config_file(db, self.base(db))
            .map(|config_file| Config::from_string(config_file.text(db).clone()))
    }

    #[salsa::tracked]
    pub fn version(self, db: &'db dyn InputDb) -> Option<Version> {
        self.config(db).map(|config| config.ingot.version).flatten()
    }

    #[salsa::tracked]
    pub fn dependencies(self, db: &'db dyn InputDb) -> Vec<(SmolStr, Url)> {
        let base_url = self.base(db);
        let mut deps = match self.config(db) {
            Some(config) => config
                .dependencies
                .into_iter()
                .map(|dependency| {
                    let mut path = match dependency.description {
                        DependencyDescription::Path(path) => path,
                        DependencyDescription::PathWithArguments { path, arguments } => path,
                    };
                    if !path.ends_with("/") {
                        path.push("");
                    }
                    let url = base_url.join(path.as_str()).unwrap().directory().unwrap();
                    (dependency.alias, url)
                })
                .collect(),
            None => vec![],
        };

        if self.kind(db) != IngotKind::Core {
            deps.push((
                "core".into(),
                Url::parse(BUILTIN_CORE_BASE_URL).expect("couldn't parse core ingot URL"),
            ))
        }
        deps

        // // Only include core dependency if not already in a core ingot
        // let core_dependency = if self.kind(db) != IngotKind::Core {
        //     vec![Dependency {
        //         alias: "core".into(),
        //         description: DependencyDescription {
        //             url: Url::parse(BUILTIN_CORE_BASE_URL).expect("couldn't parse core ingot URL"),
        //             arguments: None,
        //         },
        //     }]
        // } else {
        //     vec![]
        // };
        //
        // match self.config(db) {
        //     Some(config) => config
        //         .dependencies
        //         .into_iter()
        //         .chain(core_dependency)
        //         .collect(),
        //     None => core_dependency,
        // }
    }
}

pub trait IngotIndex {
    fn containing_ingot_base(&self, db: &dyn InputDb, location: &Url) -> Option<Url>;
    fn containing_ingot_config_file(self, db: &dyn InputDb, location: Url) -> Option<File>;
    fn containing_ingot<'db>(self, db: &'db dyn InputDb, location: &Url) -> Option<Ingot<'db>>;
    fn touch_ingot<'db>(self, db: &'db mut dyn InputDb, base_url: &Url) -> Option<Ingot<'db>>;
}

pub type Version = serde_semver::semver::Version;
#[salsa::tracked]
impl IngotIndex for Workspace {
    fn containing_ingot_base(&self, db: &dyn InputDb, location: &Url) -> Option<Url> {
        self.containing_ingot_config_file(db, location.clone())
            .map(move |config| {
                config
                    .url(db)
                    .expect("Config file should be indexed")
                    .directory()
                    .expect("Config URL should have a directory")
            })
    }
    /// Recursively search for a local ingot configuration file
    #[salsa::tracked]
    fn containing_ingot_config_file(self, db: &dyn InputDb, file: Url) -> Option<File> {
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
            Some(file_obj)
        } else {
            tracing::debug!(target: "ingot_config", "Config file NOT found in index: {}. Checking parent.", config_url);
            if let Some(parent_dir_url) = dir.parent() {
                tracing::debug!(target: "ingot_config", "Recursively calling containing_ingot_config for parent: {}", parent_dir_url);
                self.containing_ingot_config_file(db, parent_dir_url)
            } else {
                tracing::debug!(target: "ingot_config", "No parent directory for {}, stopping search.", dir);
                None
            }
        }
    }

    fn containing_ingot<'db>(self, db: &'db dyn InputDb, location: &Url) -> Option<Ingot<'db>> {
        containing_ingot_impl(db, self, location.clone())
    }

    fn touch_ingot<'db>(self, db: &'db mut dyn InputDb, base_url: &Url) -> Option<Ingot<'db>> {
        let base_dir = base_url
            .directory()
            .expect("Base URL should have a directory");
        let config_file = base_dir
            .join("fe.toml")
            .expect("Config file should be indexed");
        let config = self.touch(db, config_file, None);

        config.containing_ingot(db)
    }
}

#[salsa::tracked]
fn containing_ingot_impl<'db>(
    db: &'db dyn InputDb,
    index: Workspace,
    location: Url,
) -> Option<Ingot<'db>> {
    let is_core = location.scheme().contains("core");

    match index.containing_ingot_base(db, &location) {
        Some(ingot_url) => Some(Ingot::new(
            db,
            ingot_url,
            None,
            if is_core {
                IngotKind::Core
            } else {
                IngotKind::Local
            },
        )),
        None => {
            // Make a standalone ingot if no base is found
            let base = location.directory().unwrap_or_else(|| location.clone());
            let specific_root_file = if location.path().ends_with(".fe") {
                index.get(db, &location)
            } else {
                None
            };
            Some(Ingot::new(
                db,
                base,
                specific_root_file,
                IngotKind::StandAlone,
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
        let index = db.workspace();

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
        let found_config = index.containing_ingot_config_file(&db, url_lib);
        assert!(found_config.is_some());
        assert_eq!(found_config.and_then(|c| c.url(&db)).unwrap(), url_config);

        // Test that standalone file without a config returns None
        let no_config = index.containing_ingot_config_file(&db, url_standalone);
        assert!(no_config.is_none());
    }
}
