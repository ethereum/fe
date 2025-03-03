use std::path::{Path, PathBuf};

use super::{db::LanguageServerDatabase, get_core::get_core_ingot};
use anyhow::Result;
use common::{
    indexmap::IndexSet,
    input::{IngotDependency, IngotKind, Version},
    InputFile, InputIngot,
};

use patricia_tree::StringPatriciaMap;
use salsa::Setter;
use tracing::info;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

fn ingot_directory_key(path: String) -> String {
    path.strip_suffix(FE_CONFIG_SUFFIX)
        .unwrap_or(&path)
        .to_string()
}

pub trait IngotFileContext {
    fn get_input_for_file_path(&self, path: &str) -> Option<(InputIngot, InputFile)>;

    fn touch_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<(InputIngot, InputFile)>;

    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot>;

    fn touch_ingot_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot>;

    fn remove_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Result<()>;
}

pub struct LocalIngotContext {
    pub ingot: InputIngot,
    pub files: StringPatriciaMap<InputFile>,
}

fn ingot_contains_file(ingot_path: &str, file_path: &str) -> bool {
    let ingot_path = ingot_path
        .strip_suffix(&FE_CONFIG_SUFFIX)
        .unwrap_or(ingot_path);
    file_path.starts_with(ingot_path)
}

pub fn get_containing_ingot_mut<'a, T>(
    ingots: &'a mut StringPatriciaMap<T>,
    path: &'a str,
) -> Option<&'a mut T> {
    ingots
        .get_longest_common_prefix_mut(path)
        .filter(|(ingot_path, _)| ingot_contains_file(ingot_path, path))
        .map(|(_, ingot)| ingot)
}

pub fn get_containing_ingot<'a, T>(
    ingots: &'a StringPatriciaMap<T>,
    path: &'a str,
) -> Option<&'a T> {
    ingots
        .get_longest_common_prefix(path)
        .filter(|(ingot_path, _)| ingot_contains_file(ingot_path, path))
        .map(|(_, ingot)| ingot)
}

impl LocalIngotContext {
    pub fn new(db: &LanguageServerDatabase, config_path: &str) -> Option<Self> {
        let mut default_dependencies = IndexSet::new();
        let core = get_core_ingot(db);

        let std_library = IngotDependency::new("core", core);
        default_dependencies.insert(std_library);

        let ingot = InputIngot::new(
            db,
            config_path,
            IngotKind::Local,
            Version::new(0, 0, 0),
            default_dependencies,
        );

        Some(Self {
            ingot,
            files: StringPatriciaMap::new(),
        })
    }
}

impl IngotFileContext for LocalIngotContext {
    fn touch_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<(InputIngot, InputFile)> {
        let ingot = self.touch_ingot_for_file_path(db, path)?;
        let input = self
            .files
            .get(path)
            .copied()
            .unwrap_or_else(|| InputFile::new(db, path.into(), String::new()));
        self.files.insert(path, input);
        ingot.set_files(db, self.files.values().copied().collect());
        Some((ingot, input))
    }

    fn get_input_for_file_path(&self, path: &str) -> Option<(InputIngot, InputFile)> {
        let ingot = self.get_ingot_for_file_path(path)?;
        let file = self.files.get(path).copied()?;
        Some((ingot, file))
    }

    fn touch_ingot_for_file_path(
        &mut self,
        _db: &mut LanguageServerDatabase,
        _path: &str,
    ) -> Option<InputIngot> {
        Some(self.ingot)
    }

    fn get_ingot_for_file_path(&self, _path: &str) -> Option<InputIngot> {
        Some(self.ingot)
    }

    fn remove_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Result<()> {
        let file = self.files.remove(path);

        if let Some(_file) = file {
            let ingot = self.ingot;
            let new_ingot_files = self
                .files
                .values()
                .copied()
                .collect::<IndexSet<InputFile>>();
            ingot.set_files(db, new_ingot_files);
            Ok(())
        } else {
            Err(anyhow::anyhow!("File not found in ingot"))
        }
    }
}

pub struct StandaloneIngotContext {
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
    fn touch_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<(InputIngot, InputFile)> {
        let ingot = self.touch_ingot_for_file_path(db, path)?;
        let input_file = self
            .files
            .get(path)
            .copied()
            .unwrap_or_else(|| InputFile::new(db, path.into(), String::new()));
        let mut files = IndexSet::new();
        files.insert(input_file);
        ingot.set_files(db, files);
        ingot.set_root_file(db, input_file);
        self.files.insert(path, input_file);
        Some((ingot, input_file))
    }

    fn get_input_for_file_path(&self, path: &str) -> Option<(InputIngot, InputFile)> {
        let ingot = self.get_ingot_for_file_path(path)?;
        let file = self.files.get(path).copied()?;
        Some((ingot, file))
    }

    fn touch_ingot_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot> {
        get_containing_ingot_mut(&mut self.ingots, path)
            .as_deref()
            .copied()
            .map_or_else(
                || {
                    let ingot = InputIngot::new(
                        db,
                        path,
                        IngotKind::StandAlone,
                        Version::new(0, 0, 0),
                        IndexSet::new(),
                    );
                    self.ingots.insert(path, ingot);
                    Some(ingot)
                },
                Some,
            )
    }

    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot> {
        // this shouldn't mutate, it should only get the ingot or return `None`
        get_containing_ingot(&self.ingots, path).copied()
    }

    fn remove_input_for_file_path(
        &mut self,
        _db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Result<()> {
        let file = self.files.remove(path);
        if let Some(_file) = file {
            self.ingots.remove(path);
        }
        Ok(())
    }
}

pub struct Workspace {
    pub(crate) ingot_contexts: StringPatriciaMap<LocalIngotContext>,
    pub(crate) standalone_ingot_context: StandaloneIngotContext,
    pub(crate) root_path: Option<PathBuf>,
}

impl Workspace {
    pub fn default() -> Self {
        // let default_core_ingot = Some(get_core_ingot(db.as_input_db()));
        Self {
            ingot_contexts: StringPatriciaMap::new(),
            standalone_ingot_context: StandaloneIngotContext::new(),
            root_path: None,
        }
    }

    pub fn all_files(&self) -> impl Iterator<Item = &InputFile> {
        // Iterate over all files in the ingot contexts
        let ingot_files = self
            .ingot_contexts
            .values()
            .flat_map(|ctx| ctx.files.values());

        // Get the files from the standalone ingot context
        let standalone_files = self.standalone_ingot_context.files.values();

        // Chain the iterators to create a single iterator over all files
        ingot_files.chain(standalone_files)
    }

    pub fn set_workspace_root(
        &mut self,
        db: &mut LanguageServerDatabase,
        root_path: &Path,
    ) -> Result<()> {
        let path = root_path;
        self.root_path = Some(path.to_path_buf());
        self.sync(db)
    }

    pub fn ingot_context_from_config_path(
        &mut self,
        db: &LanguageServerDatabase,
        config_path: &str,
    ) -> Option<&mut LocalIngotContext> {
        let key = ingot_directory_key(config_path.into());
        if self.ingot_contexts.contains_key(&key) {
            return self.ingot_contexts.get_mut(&key);
        }
        let ingot_context = LocalIngotContext::new(db, config_path)?;
        self.ingot_contexts.insert(key.clone(), ingot_context);
        self.ingot_contexts.get_mut(&key)
    }

    fn sync_local_ingots(&mut self, db: &mut LanguageServerDatabase, path: &str) {
        let config_paths = glob::glob(&format!("{path}/**/{FE_CONFIG_SUFFIX}"))
            .unwrap()
            .filter_map(Result::ok)
            .map(|p| p.to_str().unwrap().to_string())
            .collect::<Vec<String>>();

        let paths = config_paths
            .iter()
            .map(|s| ingot_directory_key(s.clone()))
            .collect::<Vec<String>>();

        for path in &paths {
            self.ingot_context_from_config_path(db, path);
        }

        let existing_keys: Vec<String> = self.ingot_contexts.keys().collect();

        let keys_to_remove: Vec<String> = existing_keys
            .into_iter()
            .filter(|key| !paths.contains(key))
            .collect();

        for key in keys_to_remove {
            self.ingot_contexts.remove(&key);
        }
    }

    fn sync_ingot_files(&mut self, db: &mut LanguageServerDatabase, config_path: &str) {
        assert!(config_path.ends_with(FE_CONFIG_SUFFIX));
        info!("Syncing ingot at {}", config_path);

        let ingot_root = config_path.strip_suffix(FE_CONFIG_SUFFIX).unwrap();
        let actual_paths = glob::glob(&format!("{ingot_root}/src/**/*.fe"))
            .unwrap()
            .filter_map(Result::ok)
            .map(|p| p.to_str().unwrap().to_string())
            .collect::<Vec<String>>();

        info!("Found {} files in ingot", actual_paths.len());
        info!("Syncing ingot files: {:?}", actual_paths);

        let ingot_context = self
            .ingot_context_from_config_path(db, config_path)
            .unwrap();

        let previous_ingot_context_file_keys: Vec<String> = ingot_context.files.keys().collect();
        for path in &previous_ingot_context_file_keys {
            if !actual_paths.contains(path) {
                let _ = ingot_context.remove_input_for_file_path(db, path);
            }
        }

        for path in actual_paths {
            if !previous_ingot_context_file_keys.contains(&path) {
                if let Some((_ingot, file)) = ingot_context.touch_input_for_file_path(db, &path) {
                    if let Ok(contents) = std::fs::read_to_string(&path) {
                        file.set_text(db).to(contents);
                    }
                }
            }
        }

        let ingot_context_files = ingot_context
            .files
            .values()
            .copied()
            .collect::<IndexSet<InputFile>>();

        ingot_context.ingot.set_files(db, ingot_context_files);

        // find the root file, which is either at `./src/main.fe` or `./src/lib.fe`
        let root_file = ingot_context
            .files
            .values()
            .find(|file| {
                file.path(db).ends_with("src/main.fe") || file.path(db).ends_with("src/lib.fe")
            })
            .copied();

        if let Some(root_file) = root_file {
            info!("Setting root file for ingot: {:?}", root_file.path(db));
            ingot_context.ingot.set_root_file(db, root_file);
        }
    }
}

impl IngotFileContext for Workspace {
    fn touch_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<(InputIngot, InputFile)> {
        let ctx = get_containing_ingot_mut(&mut self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.touch_input_for_file_path(db, path)
        } else {
            self.standalone_ingot_context
                .touch_input_for_file_path(db, path)
        }
    }

    fn get_input_for_file_path(&self, path: &str) -> Option<(InputIngot, InputFile)> {
        let ctx = get_containing_ingot(&self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.get_input_for_file_path(path)
        } else {
            self.standalone_ingot_context.get_input_for_file_path(path)
        }
    }

    fn touch_ingot_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Option<InputIngot> {
        let ctx = get_containing_ingot_mut(&mut self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.touch_ingot_for_file_path(db, path)
        } else {
            self.standalone_ingot_context
                .touch_ingot_for_file_path(db, path)
        }
    }

    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot> {
        let ctx = get_containing_ingot(&self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.get_ingot_for_file_path(path)
        } else {
            self.standalone_ingot_context.get_ingot_for_file_path(path)
        }
    }

    fn remove_input_for_file_path(
        &mut self,
        db: &mut LanguageServerDatabase,
        path: &str,
    ) -> Result<()> {
        let ctx = get_containing_ingot_mut(&mut self.ingot_contexts, path);
        if let Some(ctx) = ctx {
            ctx.remove_input_for_file_path(db, path)
        } else {
            self.standalone_ingot_context
                .remove_input_for_file_path(db, path)?;
            Ok(())
        }
    }
}

pub trait SyncableIngotFileContext {
    fn sync(&mut self, db: &mut LanguageServerDatabase) -> Result<()>;
}

impl SyncableIngotFileContext for Workspace {
    fn sync(&mut self, db: &mut LanguageServerDatabase) -> Result<()> {
        let path = {
            let path = &self.root_path;
            path.clone().unwrap()
        };

        let path_str = path.to_str().unwrap();

        info!("Syncing workspace at {:?}", path_str);
        self.sync_local_ingots(db, path_str);

        let ingot_paths = glob::glob(&format!("{path_str}/**/{FE_CONFIG_SUFFIX}"))
            .ok()
            .unwrap()
            .filter_map(Result::ok)
            .filter_map(|p| p.to_str().map(std::string::ToString::to_string))
            .collect::<Vec<String>>();

        info!("Found {} ingots", ingot_paths.len());

        for ingot_path in ingot_paths {
            self.sync_ingot_files(db, &ingot_path);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use hir::lower::map_file_to_mod;

    use crate::backend::workspace::{
        get_containing_ingot_mut, IngotFileContext, Workspace, FE_CONFIG_SUFFIX,
    };
    use std::path::PathBuf;

    use super::StandaloneIngotContext;

    #[test]
    fn workspace_standalone_context() {
        let mut db = crate::backend::db::LanguageServerDatabase::default();
        let file_path = "tests/data/ingot1/src/main.fe";

        let mut ctx = StandaloneIngotContext::new();
        let file = ctx.touch_input_for_file_path(&mut db, file_path);

        assert!(file.is_some());

        let ingot = ctx.touch_ingot_for_file_path(&mut db, file_path);
        assert!(ingot.is_some());
        assert_eq!(
            ingot.unwrap().kind(&db),
            common::input::IngotKind::StandAlone
        );
    }

    #[test]
    fn test_workspace_standalone_ingot() {
        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();
        let file_path = "tests/data/ingot1/src/main.fe";
        let file = workspace.touch_input_for_file_path(&mut db, file_path);
        assert!(file.is_some());
    }

    #[test]
    fn test_get_containing_ingot() {
        let config_path = "tests/data/ingot1/fe.toml";
        let mut workspace: Workspace = Workspace::default();

        let _ingot_context_ingot = {
            let ingot_context = workspace.ingot_context_from_config_path(
                &crate::backend::db::LanguageServerDatabase::default(),
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

        let containing_ingot = get_containing_ingot_mut(&mut workspace.ingot_contexts, file_path);

        assert!(containing_ingot.as_deref().is_some());

        let ingot = workspace.touch_ingot_for_file_path(
            &mut crate::backend::db::LanguageServerDatabase::default(),
            file_path,
        );
        assert!(ingot.is_some());
    }

    #[test]
    fn test_workspace_local_ingot() {
        let config_path = "tests/data/ingot1/fe.toml";
        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();

        let ingot_context_ingot = {
            let ingot_context = workspace.ingot_context_from_config_path(&db, config_path);

            assert!(ingot_context.is_some());
            ingot_context.map(|ctx| ctx.ingot)
        };

        let file_path = "tests/data/ingot1/src/main.fe";
        let (ingot, _file) = workspace
            .touch_input_for_file_path(&mut db, file_path)
            .unwrap();

        assert_eq!(
            ingot_context_ingot.unwrap().kind(&db),
            common::input::IngotKind::Local
        );
        assert_eq!(ingot.kind(&db), common::input::IngotKind::Local);
        assert_eq!(ingot_context_ingot.unwrap(), ingot);
    }

    #[test]
    fn test_sync_single_ingot() {
        let cargo_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let ingot_base_dir =
            std::path::Path::new(&cargo_manifest_dir).join("test_files/single_ingot/");
        // let ingot_config_path = &ingot_base_dir.join("fe.toml");

        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();

        workspace
            .set_workspace_root(&mut db, &ingot_base_dir)
            .unwrap();

        assert_eq!(workspace.ingot_contexts.len(), 1);

        let fe_source_path = ingot_base_dir.join("src/main.fe");
        let (ingot, _file) = workspace
            .touch_input_for_file_path(&mut db, fe_source_path.to_str().unwrap())
            .unwrap();
        assert!(ingot.kind(&db) == common::input::IngotKind::Local);
    }

    #[test]
    fn test_sync_nested_ingots() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let path = format!("{crate_dir}/test_files/nested_ingots");
        assert!(
            glob::glob(&format!("{}/**/{}", path, FE_CONFIG_SUFFIX))
                .unwrap()
                .count()
                == 2
        );

        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &path);

        assert!(workspace.ingot_contexts.len() == 2);

        let _ = workspace.set_workspace_root(&mut db, &PathBuf::from(&path));

        // get all top level modules for .fe files in the workspace
        let fe_files = glob::glob(&format!("{path}/**/*.fe"))
            .unwrap()
            .filter_map(Result::ok)
            .map(|p| p.to_str().unwrap().to_string())
            .collect::<Vec<String>>();

        for src_path in fe_files {
            let _file = workspace
                .touch_input_for_file_path(&mut db, &src_path)
                .unwrap();
            // normally would do this but it's not relevant here...
            // file.sync(&mut db, None);

            // this would panic if a file has been added to multiple ingots
            let (ingot, file) = workspace.get_input_for_file_path(&src_path).unwrap();
            let _top_mod = map_file_to_mod(&db, ingot, file);
        }
    }

    #[test]
    fn test_sync_ingot_files() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let path = format!("{crate_dir}/test_files/nested_ingots");
        assert!(
            glob::glob(&format!("{}/**/{}", path, FE_CONFIG_SUFFIX))
                .unwrap()
                .count()
                == 2
        );

        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &path);

        assert!(workspace.ingot_contexts.len() == 2);

        let foo_config = format!("{}/ingots/foo/{}", path, FE_CONFIG_SUFFIX);
        workspace.sync_ingot_files(&mut db, &foo_config);

        let foo_context = workspace
            .ingot_context_from_config_path(&db, &foo_config)
            .unwrap();

        assert!(foo_context.files.len() == 1);

        let foo_files = foo_context.files.keys().collect::<Vec<String>>();
        for file in foo_files {
            let contents = std::fs::read_to_string(&file).unwrap();
            let (_ingot, file) = foo_context
                .touch_input_for_file_path(&mut db, &file)
                .unwrap();

            assert!(*file.text(&db) == contents);
        }
    }

    #[test]
    fn test_dangling_fe_source() {
        let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let messy_workspace_path = format!("{crate_dir}/test_files/messy");
        let dangling_path = format!("{crate_dir}/test_files/messy/dangling.fe");

        let mut workspace: Workspace = Workspace::default();
        let mut db = crate::backend::db::LanguageServerDatabase::default();

        workspace.sync_local_ingots(&mut db, &messy_workspace_path);
        let (d_ingot, _file) = workspace
            .touch_input_for_file_path(&mut db, &dangling_path)
            .unwrap();

        assert_eq!(d_ingot.kind(&db), common::input::IngotKind::StandAlone);

        // TODO: make it easier to go both ways between an ingot root path and its config path
        let ingot_paths = workspace
            .ingot_contexts
            .values()
            .map(|ctx| format!("{}{}", ctx.ingot.path(&db), FE_CONFIG_SUFFIX))
            .collect::<Vec<String>>();

        for ingot_path in ingot_paths {
            workspace.sync_ingot_files(&mut db, &ingot_path);
        }

        let non_dangling_file_path = format!("{crate_dir}/test_files/messy/foo/bar/src/main.fe");
        let (n_ingot, _file) = workspace
            .touch_input_for_file_path(&mut db, &non_dangling_file_path)
            .unwrap();

        assert_eq!(n_ingot.kind(&db), common::input::IngotKind::Local);
    }
}
