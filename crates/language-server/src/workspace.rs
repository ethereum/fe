use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use anyhow::Result;
use common::{
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use hir::{hir_def::TopLevelMod, lower::map_file_to_mod};
use log::info;
use patricia_tree::StringPatriciaMap;

use crate::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";
fn ingot_directory_key(path: String) -> String {
    path.strip_suffix(FE_CONFIG_SUFFIX)
        .unwrap_or(&path)
        .to_string()
}

pub(crate) trait IngotFileContext {
    fn input_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputFile>;
    fn ingot_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot>;
}

pub(crate) struct LocalIngotContext {
    pub ingot: InputIngot,
    pub files: StringPatriciaMap<InputFile>,
}

fn ingot_contains_file(ingot_path: &str, file_path: &str) -> bool {
    let ingot_path = ingot_path
        .strip_suffix(&FE_CONFIG_SUFFIX)
        .unwrap_or(ingot_path);
    file_path.starts_with(ingot_path)
}

pub(crate) fn get_containing_ingot<'a, T>(
    ingots: &'a mut StringPatriciaMap<T>,
    path: &'a str,
) -> Option<&'a mut T> {
    ingots
        .get_longest_common_prefix_mut(path)
        .filter(|(ingot_path, _)| ingot_contains_file(ingot_path, path))
        .map(|(_, ingot)| ingot)
}

impl LocalIngotContext {
    pub fn new(db: &LanguageServerDatabase, config_path: &str) -> Option<Self> {
        let ingot = InputIngot::new(
            db,
            config_path,
            IngotKind::Local,
            Version::new(0, 0, 0),
            BTreeSet::new(),
        );
        Some(Self {
            ingot,
            files: StringPatriciaMap::new(),
        })
    }
}

impl IngotFileContext for LocalIngotContext {
    fn input_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputFile> {
        let ingot = self.ingot_from_file_path(db, path)?;
        let input = self.files.get(path).map_or_else(
            || {
                let file = InputFile::new(db, ingot, path.into(), "".into());
                Some(file)
            },
            |file| Some(*file),
        );
        self.files.insert(path.to_string(), input.unwrap());
        input
    }

    fn ingot_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot> {
        Some(self.ingot)
    }
}

pub(crate) struct StandaloneIngotContext {
    ingots: StringPatriciaMap<InputIngot>,
    files: StringPatriciaMap<InputFile>,
}

impl StandaloneIngotContext {
    pub fn new() -> Self {
        Self {
            ingots: StringPatriciaMap::new(),
            files: StringPatriciaMap::new(),
        }
    }
}

impl IngotFileContext for StandaloneIngotContext {
    fn input_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputFile> {
        let ingot = self.ingot_from_file_path(db, path)?;
        let input_file = self.files.get(path).map_or_else(
            || {
                let file = InputFile::new(db, ingot, path.into(), "".into());
                Some(file)
            },
            |file| Some(*file),
        );
        ingot.set_files(db, [input_file.unwrap()].into());
        ingot.set_root_file(db, input_file.unwrap());
        self.files.insert(path.to_string(), input_file.unwrap());
        input_file
    }

    fn ingot_from_file_path(
        &mut self,
        _db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot> {
        get_containing_ingot(&mut self.ingots, path)
            .as_deref()
            .copied()
            .map_or_else(
                || {
                    let ingot = InputIngot::new(
                        _db,
                        path,
                        IngotKind::StandAlone,
                        Version::new(0, 0, 0),
                        BTreeSet::new(),
                    );
                    self.ingots.insert(path.to_string(), ingot);
                    Some(ingot)
                },
                |ingot| Some(ingot),
            )
    }
}

pub(crate) struct Workspace {
    pub(crate) ingot_contexts: StringPatriciaMap<LocalIngotContext>,
    pub(crate) standalone_ingot_context: StandaloneIngotContext,
    pub(crate) root_path: Option<PathBuf>,
}

impl Workspace {
    pub fn default() -> Self {
        Self {
            ingot_contexts: StringPatriciaMap::new(),
            standalone_ingot_context: StandaloneIngotContext::new(),
            root_path: None,
        }
    }

    pub fn set_workspace_root(
        &mut self,
        db: &mut LanguageServerDatabase,
        root_path: &Option<PathBuf>,
    ) -> Result<()> {
        let path = root_path.clone();
        self.root_path = path;
        self.sync(db)
    }

    pub fn ingot_context_from_config_path(
        &mut self,
        db: &LanguageServerDatabase,
        config_path: &str,
    ) -> Option<&mut LocalIngotContext> {
        let key = &ingot_directory_key(config_path.into());
        if self.ingot_contexts.contains_key(key) {
            return self.ingot_contexts.get_mut(key);
        } else {
            let ingot_context = LocalIngotContext::new(db, config_path)?;
            self.ingot_contexts
                // .insert(config_path.to_string(), ingot_context);
                // instead chop off the trailing fe.toml
                .insert(key, ingot_context);
            return self.ingot_contexts.get_mut(key);
        }
    }

    fn sync_local_ingots(&mut self, db: &mut LanguageServerDatabase, path: &str) -> () {
        let config_paths = &glob::glob(&format!("{}/**/{}", path, FE_CONFIG_SUFFIX))
            .unwrap()
            .map(|p| p.unwrap().to_str().unwrap().to_string())
            .collect::<Vec<String>>();

        let paths = &config_paths
            .into_iter()
            .map(|path| path.to_string())
            .map(ingot_directory_key)
            .collect::<Vec<String>>();

        for path in paths {
            self.ingot_context_from_config_path(db, &path);
        }

        let existing_keys: Vec<String> = self.ingot_contexts.keys().collect();

        let keys_to_remove: Vec<String> = existing_keys
            .iter()
            .filter(|key| !paths.contains(key))
            .map(|path| path.into())
            .collect();

        for key in keys_to_remove {
            self.ingot_contexts.remove(ingot_directory_key(key));
        }
    }

    fn sync_ingot_files(&mut self, db: &mut LanguageServerDatabase, config_path: &str) -> () {
        assert!(config_path.ends_with(FE_CONFIG_SUFFIX));
        info!("Syncing ingot at {}", config_path);

        let ingot_root = config_path.strip_suffix(FE_CONFIG_SUFFIX).unwrap();
        let paths = &glob::glob(&format!("{}/**/*.fe", ingot_root))
            .unwrap()
            .map(|p| p.unwrap().to_str().unwrap().to_string())
            .collect::<Vec<String>>();

        info!("Found {} files in ingot", paths.len());
        info!("Syncing ingot files: {:?}", paths);

        let ingot_context = self
            .ingot_context_from_config_path(db, config_path)
            .unwrap();

        let ingot_context_file_keys = &ingot_context.files.keys().collect::<Vec<String>>();
        ingot_context_file_keys.iter().for_each(|path| {
            if !paths.contains(&path) {
                ingot_context.files.remove(path);
            }
        });

        paths.iter().for_each(|path| {
            if !ingot_context_file_keys.contains(&path) {
                let file = ingot_context.input_from_file_path(db, path);
                let contents = std::fs::read_to_string(path).unwrap();
                file.unwrap().set_text(db).to(contents);
            }
        });

        let ingot_context_files = ingot_context
            .files
            .values()
            .map(|x| *x)
            .collect::<BTreeSet<InputFile>>();

        ingot_context.ingot.set_files(db, ingot_context_files);

        // find the root file, which is either at `./src/main.fe` or `./src/lib.fe`
        let root_file = ingot_context
            .files
            .values()
            .find(|file| {
                file.path(db).ends_with("src/main.fe") || file.path(db).ends_with("src/lib.fe")
            })
            .map(|file| *file);

        if let Some(root_file) = root_file {
            info!("Setting root file for ingot: {:?}", root_file.path(db));
            ingot_context.ingot.set_root_file(db, root_file);
        }
    }

    pub fn top_mod_from_file(
        &mut self,
        db: &mut LanguageServerDatabase,
        file_path: &Path,
        source: &str,
    ) -> TopLevelMod {
        let file = self
            .input_from_file_path(db, file_path.to_str().unwrap())
            .unwrap();
        file.set_text(db).to(source.to_string());
        // let ingot = file.ingot(db);
        // let mut files = ingot.files(db).clone();
        // files.insert(file);
        // ingot.set_files(db, files);
        map_file_to_mod(db, file)
    }
}

impl IngotFileContext for Workspace {
    fn input_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputFile> {
        let ctx = get_containing_ingot(&mut self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.input_from_file_path(db, path)
        } else {
            (&mut self.standalone_ingot_context).input_from_file_path(db, path)
        }
    }

    fn ingot_from_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot> {
        let ctx = get_containing_ingot(&mut self.ingot_contexts, path);
        if ctx.is_some() {
            Some(ctx.unwrap().ingot_from_file_path(db, path).unwrap())
        } else {
            (&mut self.standalone_ingot_context).ingot_from_file_path(db, path)
        }
    }
}

pub(crate) trait SyncableIngotFileContext {
    fn sync(&mut self, db: &mut LanguageServerDatabase) -> Result<()>;
}

impl SyncableIngotFileContext for Workspace {
    fn sync(&mut self, db: &mut LanguageServerDatabase) -> Result<()> {
        let path = {
            let path = &self.root_path;
            path.clone().unwrap()
        };
        
        let path = path.to_str().unwrap();

        info!("Syncing workspace at {:?}", path);
        self.sync_local_ingots(db, path);

        let ingot_paths = glob::glob(&format!("{}/**/{}", path, FE_CONFIG_SUFFIX))
            .ok()
            .unwrap()
            .filter_map(Result::ok)
            .filter_map(|p| p.to_str().map(|s| s.to_string()))
            .collect::<Vec<String>>();

        info!("Found {} ingots", ingot_paths.len());

        for ingot_path in ingot_paths {
            self.sync_ingot_files(db, &ingot_path);
        }

        let paths = glob::glob(&format!("{}/**/*.fe", path))
            .ok()
            .unwrap()
            .filter_map(|p| p.ok().unwrap().to_str().map(|s| s.to_string()))
            .collect::<Vec<String>>();

        for path in paths {
            self.input_from_file_path(db, &path);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use glob::glob;

    use crate::workspace::{get_containing_ingot, IngotFileContext, Workspace, FE_CONFIG_SUFFIX};

    use super::StandaloneIngotContext;

    #[test]
    fn test_standalone_context() {
        let mut db = crate::db::LanguageServerDatabase::default();
        let file_path = "tests/data/ingot1/src/main.fe";

        let ctx = &mut StandaloneIngotContext::new();
        let file = ctx.input_from_file_path(&mut db, file_path);

        assert!(file.is_some());

        let ingot = ctx.ingot_from_file_path(&mut db, file_path);
        assert!(ingot.is_some());
        assert_eq!(
            ingot.unwrap().kind(&mut db),
            common::input::IngotKind::StandAlone
        );
        assert_eq!(ingot.unwrap(), file.unwrap().ingot(&mut db));
    }

    #[test]
    fn test_workspace_standalone_ingot() {
        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();
        let file_path = "tests/data/ingot1/src/main.fe";
        let file = workspace.input_from_file_path(&mut db, file_path);
        assert!(file.is_some());
    }

    #[test]
    fn test_get_containing_ingot() {
        let config_path = "tests/data/ingot1/fe.toml";
        let mut workspace = Workspace::default();

        let _ingot_context_ingot = {
            let ingot_context = workspace.ingot_context_from_config_path(
                &mut crate::db::LanguageServerDatabase::default(),
                config_path,
            );

            assert!(ingot_context.is_some());
            ingot_context.map(|ctx| ctx.ingot)
        };

        assert!(workspace.ingot_contexts.len() == 1);

        let file_path = "tests/data/ingot1/src/main.fe";
        assert!(workspace
            .ingot_contexts
            .get_longest_common_prefix(file_path)
            .is_some());

        let containing_ingot = get_containing_ingot(&mut workspace.ingot_contexts, file_path);

        assert!(containing_ingot.as_deref().is_some());

        let ingot = workspace
            .ingot_from_file_path(&mut crate::db::LanguageServerDatabase::default(), file_path);
        assert!(ingot.is_some());
    }

    #[test]
    fn test_workspace_local_ingot() {
        let config_path = "tests/data/ingot1/fe.toml";
        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();

        let ingot_context_ingot = {
            let ingot_context = workspace.ingot_context_from_config_path(&mut db, config_path);

            assert!(ingot_context.is_some());
            ingot_context.map(|ctx| ctx.ingot)
        };

        let file_path = "tests/data/ingot1/src/main.fe";
        let file = workspace.input_from_file_path(&mut db, file_path);
        assert!(file.is_some());

        let ingot = workspace.ingot_from_file_path(&mut db, file_path);
        assert!(ingot.is_some());

        assert_eq!(file.map(|f| f.ingot(&mut db)).unwrap(), ingot.unwrap());

        assert_eq!(
            ingot_context_ingot.unwrap().kind(&mut db),
            common::input::IngotKind::Local
        );
        assert_eq!(
            ingot.unwrap().kind(&mut db),
            common::input::IngotKind::Local
        );
        assert_eq!(ingot_context_ingot.unwrap(), ingot.unwrap());
    }
    
    #[test]
    fn test_sync_single_ingot() {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir = std::path::Path::new(&cargo_manifest_dir).join("test_files/single_ingot/");
        let ingot_config_path = &ingot_base_dir.join("fe.toml");
        
        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();

        let _ = workspace.set_workspace_root(&mut db, &Some(ingot_base_dir.clone()));
        // panic!("wtf? {:?}", ingot_base_dir);

        assert_eq!(workspace.ingot_contexts.len(), 1);

        let fe_source_path = ingot_base_dir.join("src/main.fe");
        let input = workspace.input_from_file_path(&mut db, fe_source_path.to_str().unwrap());
        assert!(input.is_some());
        assert!(input.unwrap().ingot(&mut db).kind(&mut db) == common::input::IngotKind::Local);
    }

    #[test]
    fn test_sync_nested_ingots() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let path = format!("{}/test_files/nested_ingots", crate_dir);
        assert!(
            glob::glob(&format!("{}/**/{}", path, super::FE_CONFIG_SUFFIX))
                .unwrap()
                .count()
                == 2
        );

        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &path);

        assert!(workspace.ingot_contexts.len() == 2);
    }

    #[test]
    fn test_sync_ingot_files() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let path = format!("{}/test_files/nested_ingots", crate_dir);
        assert!(
            glob::glob(&format!("{}/**/{}", path, super::FE_CONFIG_SUFFIX))
                .unwrap()
                .count()
                == 2
        );

        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &path);

        assert!(workspace.ingot_contexts.len() == 2);

        let foo_config = format!("{}/ingots/foo/{}", path, super::FE_CONFIG_SUFFIX);
        workspace.sync_ingot_files(&mut db, &foo_config);

        let foo_context = workspace
            .ingot_context_from_config_path(&db, &foo_config)
            .unwrap();

        assert!(foo_context.files.len() == 1);

        let foo_files = foo_context.files.keys().collect::<Vec<String>>();
        for file in foo_files {
            let contents = std::fs::read_to_string(&file).unwrap();
            let file = foo_context.input_from_file_path(&mut db, &file).unwrap();

            assert!(*file.text(&mut db) == contents);
        }
    }

    #[test]
    fn test_dangling_fe_source() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let messy_workspace_path = format!("{}/test_files/messy", crate_dir);
        let dangling_path = format!("{}/test_files/messy/dangling.fe", crate_dir);

        let mut workspace = Workspace::default();
        let mut db = crate::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &messy_workspace_path);
        let dangling_file = workspace
            .input_from_file_path(&mut db, &dangling_path)
            .unwrap();

        assert_eq!(
            dangling_file.ingot(&db).kind(&mut db),
            common::input::IngotKind::StandAlone
        );

        // TODO: make it easier to go both ways between an ingot root path and its config path
        let ingot_paths = workspace
            .ingot_contexts
            .values()
            .map(|ctx| {
                format!(
                    "{}{}",
                    ctx.ingot.path(&mut db).to_string(),
                    FE_CONFIG_SUFFIX
                )
            })
            .collect::<Vec<String>>();

        for ingot_path in ingot_paths {
            workspace.sync_ingot_files(&mut db, &ingot_path);
        }

        let non_dangling_file_path = format!("{}/test_files/messy/foo/bar/src/main.fe", crate_dir);
        let non_dangling_input = workspace
            .input_from_file_path(&mut db, &non_dangling_file_path)
            .unwrap();

        assert_eq!(
            non_dangling_input.ingot(&db).kind(&mut db),
            common::input::IngotKind::Local
        );
    }
}
