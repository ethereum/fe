use std::{collections::BTreeSet, path::Path};

use common::{
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use hir::{hir_def::TopLevelMod, lower::map_file_to_mod};
use patricia_tree::StringPatriciaMap;

use crate::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";
fn ingot_directory_key(path: &str) -> String {
    path.strip_suffix(FE_CONFIG_SUFFIX).unwrap_or(path).to_string()
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
}

impl Workspace {
    pub fn default() -> Self {
        Self {
            ingot_contexts: StringPatriciaMap::new(),
            standalone_ingot_context: StandaloneIngotContext::new(),
        }
    }

    pub fn ingot_context_from_config_path(
        &mut self,
        db: &LanguageServerDatabase,
        config_path: &str,
    ) -> Option<&LocalIngotContext> {
        let key = &ingot_directory_key(config_path);
        if self.ingot_contexts.contains_key(key) {
            return self.ingot_contexts.get(key);
        } else {
            let ingot_context = LocalIngotContext::new(db, config_path)?;
            self.ingot_contexts
                // .insert(config_path.to_string(), ingot_context);
                // instead chop off the trailing fe.toml
                .insert(key, ingot_context);
            return self.ingot_contexts.get(key);
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
        let ingot = file.ingot(db);
        let mut files = ingot.files(db).clone();
        files.insert(file);
        ingot.set_files(db, files);
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

#[cfg(test)]
mod tests {
    use crate::workspace::{IngotFileContext, Workspace, get_containing_ingot};

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
            let ingot_context = workspace
            .ingot_context_from_config_path(&mut crate::db::LanguageServerDatabase::default(), config_path);
            
            assert!(ingot_context.is_some());
            ingot_context.map(|ctx| ctx.ingot)
        };
        
        assert!(workspace.ingot_contexts.len() == 1);
        
        let file_path = "tests/data/ingot1/src/main.fe";
        assert!(workspace.ingot_contexts.get_longest_common_prefix(file_path).is_some());
        
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
            let ingot_context = workspace
            .ingot_context_from_config_path(&mut db, config_path);
            
            assert!(ingot_context.is_some());
            ingot_context.map(|ctx| ctx.ingot)
        };

        let file_path = "tests/data/ingot1/src/main.fe";
        let file = workspace
            .input_from_file_path(&mut db, file_path);
        assert!(file.is_some());

        let ingot = workspace
            .ingot_from_file_path(&mut db, file_path);
        assert!(ingot.is_some());
        
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
}
