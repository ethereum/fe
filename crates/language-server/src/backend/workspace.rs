use std::path::{Path, PathBuf};

use anyhow::Result;
use common::InputDb;
use tracing::info;
use url::Url;

use super::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

/// Manages the workspace files and ingots for the language server
pub struct Workspace {
    pub(crate) root_path: Option<PathBuf>,
}

impl Workspace {
    pub fn default() -> Self {
        Self { root_path: None }
    }

    /// Sets the workspace root path and syncs workspace files
    pub fn set_workspace_root(
        &mut self,
        db: &mut LanguageServerDatabase,
        root_path: &Path,
    ) -> Result<()> {
        self.root_path = Some(root_path.to_path_buf());
        self.sync(db)
    }

    /// Returns all files in the workspace
    pub fn all_files<'db>(
        &self,
        db: &'db dyn InputDb,
    ) -> impl Iterator<Item = common::file::File> + 'db {
        db.workspace().all_files(db).iter().map(|(_url, file)| file)
    }

    /// Syncs the workspace with the file system
    pub fn sync(&mut self, db: &mut dyn InputDb) -> Result<()> {
        let path = match &self.root_path {
            Some(path) => path,
            None => return Ok(()),
        };

        let path_str = path.to_str().unwrap_or_default().to_string();
        info!("Syncing workspace at {:?}", path_str);

        // Find fe.toml files and sync them
        self.sync_local_ingots(db, &path_str)?;

        // Sync files for each ingot
        let ingot_paths = glob::glob(&format!("{}/**/{}", path_str, FE_CONFIG_SUFFIX))?
            .filter_map(Result::ok)
            .filter_map(|p| p.to_str().map(String::from))
            .collect::<Vec<_>>();

        for ingot_path in &ingot_paths {
            self.sync_ingot_files(db, ingot_path)?;
        }

        Ok(())
    }

    /// Syncs all local ingots in the workspace
    fn sync_local_ingots(&mut self, db: &mut dyn InputDb, workspace_path: &str) -> Result<()> {
        // Find and touch all fe.toml files
        let config_paths = glob::glob(&format!("{workspace_path}/**/{FE_CONFIG_SUFFIX}"))?
            .filter_map(Result::ok)
            .map(Url::from_file_path)
            .filter_map(Result::ok);

        for path in config_paths {
            db.workspace().touch(db, path, None);
        }
        Ok(())
    }

    /// Syncs files for a specific ingot
    fn sync_ingot_files(&mut self, db: &mut dyn InputDb, config_path: &str) -> Result<()> {
        let ingot_root = config_path.strip_suffix(FE_CONFIG_SUFFIX).unwrap();
        info!("Syncing ingot at {}", config_path);

        // Find all Fe source files
        let fe_files = glob::glob(&format!("{ingot_root}/src/**/*.fe"))?
            .filter_map(Result::ok)
            .map(Url::from_file_path)
            .filter_map(Result::ok)
            .collect::<Vec<_>>();

        // Get ingot URL and descriptor
        let ingot_url =
            Url::from_file_path(ingot_root).map_err(|_| anyhow::anyhow!("Invalid URL"))?;
        let ingot = match db.workspace().containing_ingot(db, &ingot_url) {
            Some(ingot) => ingot,
            None => return Ok(()),
        };

        // Track existing files and remove those no longer on disk
        let existing_files: Vec<Url> = ingot.files(db).iter().map(|(url, _)| url).collect();

        for url in existing_files {
            if !fe_files.contains(&url) {
                db.workspace().remove(db, &url);
            }
        }

        // Add new files
        for url in fe_files {
            if db.workspace().get(db, &url).is_none() {
                if let Ok(contents) =
                    std::fs::read_to_string(url.to_file_path().expect("couldn't read file"))
                {
                    db.workspace().touch(db, url, Some(contents));
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::ingot::IngotKind;
    use common::ingot::Version;
    use common::InputDb;
    use hir::lower::map_file_to_mod;
    use std::fs::create_dir_all;
    use std::fs::File as StdFile;
    use std::io::Write;
    use tempfile::TempDir;

    fn setup_test_fs() -> (TempDir, PathBuf) {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path().to_path_buf();

        // Create ingot1 structure
        let ingot1_path = root_path.join("ingot1");
        create_dir_all(ingot1_path.join("src")).unwrap();

        let mut config_file = StdFile::create(ingot1_path.join("fe.toml")).unwrap();
        write!(
            config_file,
            "[ingot]\nname = \"ingot1\"\nversion = \"0.1.0\"\n"
        )
        .unwrap();

        let mut main_file = StdFile::create(ingot1_path.join("src").join("main.fe")).unwrap();
        write!(
            main_file,
            "contract Test {{\n    pub fn test() -> u256 {{\n        return 42\n    }}\n}}\n"
        )
        .unwrap();

        // Create ingot2 in a subdirectory
        let ingot2_path = root_path.join("nested").join("ingot2");
        create_dir_all(ingot2_path.join("src")).unwrap();

        let mut config_file = StdFile::create(ingot2_path.join("fe.toml")).unwrap();
        write!(
            config_file,
            "[ingot]\nname = \"ingot2\"\nversion = \"0.1.0\"\n"
        )
        .unwrap();

        let mut lib_file = StdFile::create(ingot2_path.join("src").join("lib.fe")).unwrap();
        write!(
            lib_file,
            "contract Test2 {{\n    pub fn test2() -> u256 {{\n        return 84\n    }}\n}}\n"
        )
        .unwrap();

        (temp_dir, root_path)
    }

    #[test]
    fn test_workspace_sync() {
        let (_temp_dir, root_path) = setup_test_fs();
        let mut db = LanguageServerDatabase::default();
        let mut workspace = Workspace::default();

        // Set workspace root and sync
        workspace.set_workspace_root(&mut db, &root_path).unwrap();

        // Check that ingots were discovered
        let ingot1_path = root_path.join("ingot1").join("fe.toml");
        let ingot2_path = root_path.join("nested").join("ingot2").join("fe.toml");

        // Find ingot1 in test_database from database passed to test
        let ingot1_url = Url::from_file_path(ingot1_path.parent().unwrap()).unwrap();
        let ingot2_url = Url::from_file_path(ingot2_path.parent().unwrap()).unwrap();

        let ingot1_desc = db.workspace().containing_ingot(&db, &ingot1_url);
        let ingot2_desc = db.workspace().containing_ingot(&db, &ingot2_url);

        assert!(ingot1_desc.is_some(), "Ingot1 should be discovered");
        assert!(ingot2_desc.is_some(), "Ingot2 should be discovered");

        // Verify that the ingot metadata is the default, as we are not parsing fe.toml for it
        if let Some(ingot) = ingot1_desc {
            assert_eq!(
                ingot.version(&db),
                Version::new(0, 0, 0),
                "Ingot1 version should be default"
            );
        }
        if let Some(ingot) = ingot2_desc {
            assert_eq!(
                ingot.version(&db),
                Version::new(0, 0, 0),
                "Ingot2 version should be default"
            );
        }

        // Check that files were discovered
        let main_path = root_path.join("ingot1").join("src").join("main.fe");
        let lib_path = root_path
            .join("nested")
            .join("ingot2")
            .join("src")
            .join("lib.fe");

        // Check that fe.toml files are also touched
        let config1_url = Url::from_file_path(&ingot1_path).unwrap();
        let config2_url = Url::from_file_path(&ingot2_path).unwrap();

        db.workspace().touch(&mut db, config1_url, None);
        db.workspace().touch(&mut db, config2_url, None);

        let main_url = Url::from_file_path(&main_path).unwrap();
        let lib_url = Url::from_file_path(&lib_path).unwrap();

        let main_file = db.workspace().get(&db, &main_url);
        let lib_file = db.workspace().get(&db, &lib_url);

        assert!(main_file.is_some(), "main.fe should be discovered");
        assert!(lib_file.is_some(), "lib.fe should be discovered");

        // Verify file contents were loaded correctly
        let main_file = main_file.unwrap();
        let lib_file = lib_file.unwrap();

        assert!(main_file.text(&db).contains("contract Test"));
        assert!(lib_file.text(&db).contains("contract Test2"));

        // Test mapping files to mods
        let main_mod = map_file_to_mod(&db, main_file);
        let lib_mod = map_file_to_mod(&db, lib_file);

        assert_eq!(main_mod.ingot(&db).kind(&db), IngotKind::Local);
        assert_eq!(lib_mod.ingot(&db).kind(&db), IngotKind::Local);
    }

    #[test]
    fn test_standalone_file() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path().to_path_buf();
        let file_path = root_path.join("standalone.fe");

        // Create a standalone file
        let mut file = StdFile::create(&file_path).unwrap();
        write!(
            file,
            "contract Standalone {{\n    pub fn test() -> u256 {{\n        return 99\n    }}\n}}\n"
        )
        .unwrap();

        let mut db = LanguageServerDatabase::default();

        // Touch the file directly using Workspace
        let file_url = Url::from_file_path(&file_path).unwrap();
        // Read the actual file content from disk
        let file_content = std::fs::read_to_string(&file_path).unwrap();
        let file = db
            .workspace()
            .touch(&mut db, file_url.clone(), Some(file_content));
        assert!(file.text(&db).contains("contract Standalone"));

        // Get the ingot for the file
        let ingot = db
            .workspace()
            .containing_ingot(&db, &file_url)
            .expect("Ingot should be created for standalone file");
        assert_eq!(ingot.kind(&db), IngotKind::StandAlone);

        // Test mapping file to mod
        let top_mod = map_file_to_mod(&db, file);
        assert_eq!(top_mod.ingot(&db).kind(&db), IngotKind::StandAlone);
    }

    #[test]
    fn test_file_removal() {
        let (_temp_dir, root_path) = setup_test_fs();
        let mut db = LanguageServerDatabase::default();
        let mut workspace = Workspace::default();

        // Set workspace root and sync
        workspace.set_workspace_root(&mut db, &root_path).unwrap();

        // Get a file
        let main_path = root_path.join("ingot1").join("src").join("main.fe");

        // Remove the file
        let main_url_to_remove = Url::from_file_path(&main_path).unwrap();
        db.workspace().remove(&mut db, &main_url_to_remove);

        // Verify the file is gone
        let main_url = Url::from_file_path(&main_path).unwrap();
        let main_file = db.workspace().get(&db, &main_url);
        assert!(main_file.is_none(), "File should be removed");
    }
}
