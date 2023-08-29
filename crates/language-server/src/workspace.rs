use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use common::{
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use patricia_tree::StringPatriciaMap;

use crate::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

trait IngotFileContext {
    fn get_input_for_file_path(&self, path: &str) -> Option<InputFile>;
    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot>;
}

struct Ingot {
    local: InputIngot,
    external: InputIngot,
}

pub struct LocalIngotContext {
    db: LanguageServerDatabase,
    // cache `InputIngot` for path
    ingot: InputIngot,
    external_ingots: StringPatriciaMap<InputIngot>,
    // cache `InputFile` for path
    files: StringPatriciaMap<InputFile>,
}

fn ingot_contains_file(ingot_path: &str, file_path: &str) -> bool {
    let ingot_path = ingot_path
        .strip_suffix(&FE_CONFIG_SUFFIX)
        .unwrap_or(ingot_path);
    file_path.starts_with(ingot_path)
}

fn get_containing_ingot<'a, T>(ingots: &'a StringPatriciaMap<T>, path: &'a str) -> Option<T> {
    ingots
        .get_longest_common_prefix(path)
        .filter(|(ingot_path, _)| ingot_contains_file(ingot_path, path))
        .map(|(_, ingot)| *ingot)
}

impl LocalIngotContext {
    pub fn new(db: LanguageServerDatabase, config_path: &str) -> Option<Self> {
        let ingot = InputIngot::new(
            &db,
            config_path,
            IngotKind::Local,
            Version::new(0, 0, 0),
            BTreeSet::new(),
        );
        Some(Self {
            db,
            ingot,
            external_ingots: StringPatriciaMap::new(),
            files: StringPatriciaMap::new(),
        })
    }
}

impl IngotFileContext for LocalIngotContext {
    fn get_input_for_file_path(&self, path: &str) -> Option<InputFile> {
        self.files.get(path).map_or_else(
            || {
                let ingot = self.get_ingot_for_file_path(path)?;
                let file = InputFile::new(&self.db, ingot, path.into(), "".into());
                self.files.insert(path.to_string(), file.clone());
                Some(file)
            },
            |file| Some(*file),
        )
    }

    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot> {
        get_containing_ingot(&self.external_ingots, path).or_else(|| Some(self.ingot.clone()))
    }
}

struct StandaloneIngotContext {
    db: LanguageServerDatabase,
    ingots: StringPatriciaMap<InputIngot>,
    files: StringPatriciaMap<InputFile>,
}

impl StandaloneIngotContext {
    pub fn new(db: LanguageServerDatabase) -> Self {
        Self {
            db,
            ingots: StringPatriciaMap::new(),
            files: StringPatriciaMap::new(),
        }
    }
}

impl IngotFileContext for StandaloneIngotContext {
    fn get_input_for_file_path(&self, path: &str) -> Option<InputFile> {
        self.files.get(path).map_or_else(
            || {
                let ingot = self.get_ingot_for_file_path(path)?;
                let file = InputFile::new(&self.db, ingot, path.into(), "".into());
                self.files.insert(path.to_string(), file.clone());
                Some(file)
            },
            |file| Some(*file),
        )
    }

    fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot> {
        get_containing_ingot(&self.ingots, path)
    }
}

struct Workspace {
    db: LanguageServerDatabase,
    ingot_contexts: StringPatriciaMap<LocalIngotContext>,
    standalone_ingot_contexts: StandaloneIngotContext,
}

impl Workspace {
    pub fn new(db: LanguageServerDatabase) -> Self {
        Self {
            db,
            ingot_contexts: StringPatriciaMap::new(),
            standalone_ingot_contexts: StandaloneIngotContext::new(db),
        }
    }

    pub fn get_ingot_context(&self, config_path: &str) -> Option<LocalIngotContext> {
        self.ingot_contexts.get(config_path).map_or_else(
            || {
                let ingot = InputIngot::new(
                    &self.db,
                    config_path,
                    IngotKind::Local,
                    Version::new(0, 0, 0),
                    BTreeSet::new(),
                );
                let context = LocalIngotContext::new(self.db, config_path);
                self.ingot_contexts
                    .insert(config_path.to_string(), context.unwrap());
                context
            },
            |context| Some(*context),
        )
    }

    pub fn get_ingot_for_file_path(&self, path: &str) -> Option<InputIngot> {
        get_containing_ingot(&self.ingot_contexts, path).map_or_else(
            || self.standalone_ingot_contexts.get_ingot_for_file_path(path),
            |ingot| Some(ingot.ingot),
        )
    }

    pub fn get_input_file_for_file_path(&self, path: &str) -> Option<InputFile> {
        self.get_ingot_for_file_path(path)
            .map_or_else(|| None, |ingot| Some(ingot.root_file(&self.db)))
    }
}
