use std::collections::BTreeSet;

use common::{
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use patricia_tree::StringPatriciaMap;

use crate::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

trait IngotFileContext {
    fn get_input_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputFile>;
    fn get_ingot_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputIngot>;
}

struct Ingot {
    local: InputIngot,
    external: InputIngot,
}

pub struct LocalIngotContext {
    ingot: InputIngot,
    // external_ingots: StringPatriciaMap<InputIngot>,
    // cache `InputFile` for path
    files: StringPatriciaMap<InputFile>,
}

fn ingot_contains_file(ingot_path: &str, file_path: &str) -> bool {
    let ingot_path = ingot_path
        .strip_suffix(&FE_CONFIG_SUFFIX)
        .unwrap_or(ingot_path);
    file_path.starts_with(ingot_path)
}

fn get_containing_ingot<'a, T>(ingots: &'a StringPatriciaMap<T>, path: &'a str) -> Option<&'a T> {
    ingots
        .get_longest_common_prefix(path)
        .filter(|(ingot_path, _)| ingot_contains_file(ingot_path, path))
        .map(|(_, ingot)| ingot)
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
            ingot,
            // external_ingots: StringPatriciaMap::new(),
            files: StringPatriciaMap::new(),
        })
    }
}

impl IngotFileContext for LocalIngotContext {
    fn get_input_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputFile> {
        let ingot = self.get_ingot_for_file_path(db, path)?;
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

    fn get_ingot_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputIngot> {
        Some(self.ingot)
    }
}

struct StandaloneIngotContext {
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
    fn get_input_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputFile> {
        let ingot = self.get_ingot_for_file_path(db, path)?;
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

    fn get_ingot_for_file_path(&mut self, _db: &LanguageServerDatabase, path: &str) -> Option<InputIngot> {
        get_containing_ingot(&self.ingots, path).as_deref().copied()
    }
}

pub(crate) struct Workspace {
    ingot_contexts: StringPatriciaMap<LocalIngotContext>,
    standalone_ingot_contexts: StandaloneIngotContext,
}

impl Workspace {
    pub fn new() -> Self {
        Self {
            ingot_contexts: StringPatriciaMap::new(),
            standalone_ingot_contexts: StandaloneIngotContext::new(),
        }
    }

    pub fn get_ingot_context(&mut self, db: LanguageServerDatabase, config_path: &str) -> Option<&LocalIngotContext> {
        if self.ingot_contexts.contains_key(config_path) {
            return self.ingot_contexts.get(config_path);
        } else {
            let ingot_context = LocalIngotContext::new(db, config_path)?;
            self.ingot_contexts.insert(config_path.to_string(), ingot_context);
            return self.ingot_contexts.get(config_path);
        }
    }

    pub fn get_ingot_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputIngot> {
        let ctx = get_containing_ingot(&self.ingot_contexts, path);
        ctx.map_or_else(
            || self.standalone_ingot_contexts.get_ingot_for_file_path(db, path),
            |ingot_context| Some(ingot_context.ingot.clone()),
        )
    }

    pub fn get_input_file_for_file_path(&mut self, db: &LanguageServerDatabase, path: &str) -> Option<InputFile> {
        self.get_ingot_for_file_path(db, path)
            .map_or_else(|| None, |ingot| Some(ingot.root_file(db)))
    }
}
