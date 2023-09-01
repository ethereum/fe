use std::{collections::BTreeSet, path::Path};

use common::{
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use hir::{hir_def::TopLevelMod, lower::map_file_to_mod};
use patricia_tree::StringPatriciaMap;

use crate::db::LanguageServerDatabase;

const FE_CONFIG_SUFFIX: &str = "fe.toml";

pub(crate) trait IngotFileContext {
    fn get_input_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputFile>;
    fn get_ingot_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputIngot>;
}

struct Ingot {
    local: InputIngot,
    external: InputIngot,
}

// derive `Copy` for `Ingot` because `StringPatriciaMap` requires `Copy` for its value type.
pub(crate) struct LocalIngotContext {
    pub ingot: InputIngot,
    // external_ingots: StringPatriciaMap<InputIngot>,
    // cache `InputFile` for path
    pub files: StringPatriciaMap<InputFile>,
}

fn ingot_contains_file(ingot_path: &str, file_path: &str) -> bool {
    let ingot_path = ingot_path
        .strip_suffix(&FE_CONFIG_SUFFIX)
        .unwrap_or(ingot_path);
    file_path.starts_with(ingot_path)
}

pub(crate) fn get_containing_ingot<'a, T>(ingots: &'a mut StringPatriciaMap<T>, path: &'a str) -> Option<&'a mut T> {
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
            // external_ingots: StringPatriciaMap::new(),
            files: StringPatriciaMap::new(),
        })
    }
}

impl IngotFileContext for LocalIngotContext {
    fn get_input_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputFile> {
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

    fn get_ingot_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputIngot> {
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
    fn get_input_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputFile> {
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

    fn get_ingot_for_file_path(&mut self, _db: &mut LanguageServerDatabase, path: &str) -> Option<InputIngot> {
        get_containing_ingot(&mut self.ingots, path).as_deref().copied()
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

    pub fn get_ingot_context(&mut self, db: &LanguageServerDatabase, config_path: &str) -> Option<&LocalIngotContext> {
        if self.ingot_contexts.contains_key(config_path) {
            return self.ingot_contexts.get(config_path);
        } else {
            let ingot_context = LocalIngotContext::new(db, config_path)?;
            self.ingot_contexts.insert(config_path.to_string(), ingot_context);
            return self.ingot_contexts.get(config_path);
        }
    }

    pub fn top_mod_from_file(&mut self, db: &mut LanguageServerDatabase, file_path: &Path, source: &str) -> TopLevelMod {
        // let workspace = &mut self.workspace;
        // create a new scope in which `self` is not mutable:
        let file = self.get_input_for_file_path(db, file_path.to_str().unwrap()).unwrap();
        file.set_text(db).to(source.to_string());
        // use salsa2022 setter to set the file's `text` field
        let ingot = file.ingot(db);
        let mut files = ingot.files(db).clone();
        files.insert(file);
        ingot.set_files(db, files);
        map_file_to_mod(db, file)
    }

}

impl IngotFileContext for Workspace {
    fn get_input_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputFile> {
        let ctx = get_containing_ingot(&mut self.ingot_contexts, path);
        if ctx.is_some() {
            Some(ctx.unwrap().get_input_for_file_path(db, path).unwrap())
        } else {
            (&mut self.standalone_ingot_context).get_input_for_file_path(db, path)
        }
    }

    fn get_ingot_for_file_path(&mut self, db: &mut LanguageServerDatabase, path: &str) -> Option<InputIngot> {
        let ctx = get_containing_ingot(&mut self.ingot_contexts, path);
        if ctx.is_some() {
            Some(ctx.unwrap().get_ingot_for_file_path(db, path).unwrap())
        } else {
            (&mut self.standalone_ingot_context).get_ingot_for_file_path(db, path)
        }
    }
}