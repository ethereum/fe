use std::path::Path;

use blart::{AsBytes, NoPrefixesBytes, TreeMap};
use camino::Utf8PathBuf;

use crate::{
    indexmap::{IndexMap, IndexSet},
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};

#[salsa::input(constructor=__new_impl)]
pub struct IngotWorkspace {
    ingots: IndexSet<InputIngot>,
    root: Option<Utf8PathBuf>,
}

#[salsa::tracked]
impl<'a> IngotWorkspace {
    pub fn new(db: &'a dyn InputDb) -> Self {
        let ingots = IndexSet::new();
        IngotWorkspace::__new_impl(db, ingots, None)
    }

    #[salsa::tracked]
    pub fn index(self, db: &'a dyn InputDb) -> IngotIndex<'a> {
        let mut map = TreeMap::new();
        let mut core_ingots = IndexMap::new();
        for ingot in self.ingots(db) {
            if ingot.kind(db) == IngotKind::Core {
                core_ingots.insert(ingot.version(db).clone(), ingot.clone());
            }
            map.insert(IngotPath::from(ingot.path(db)), ingot);
        }
        IngotIndex::new(db, map, core_ingots)
    }

    #[salsa::tracked]
    pub fn get_ingot(
        self,
        db: &'a dyn InputDb,
        ingot_or_file_path: &'a Utf8PathBuf,
    ) -> Option<InputIngot> {
        let ingot_map = self.index(db);
        let map = ingot_map.map(db);
        map.fuzzy(&IngotPath::from(ingot_or_file_path), 2)
            .next()
            .map_or(None, |(ingot_path, ingot)| {
                if ingot_or_file_path.starts_with(ingot_path) {
                    Some(ingot.clone())
                } else {
                    None
                }
            })
    }

    #[salsa::tracked]
    pub fn get_file(self, db: &'a dyn InputDb, file_path: &'a Utf8PathBuf) -> Option<InputFile> {
        let ingot = self.get_ingot(db, file_path)?;
        ingot.get_file(db, &file_path)
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct IngotIndex<'db> {
    #[salsa::as_ref]
    map: TreeMap<IngotPath, InputIngot>,
    core_ingots: IndexMap<Version, InputIngot>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IngotPath(Utf8PathBuf);
impl From<&Utf8PathBuf> for IngotPath {
    fn from(path: &Utf8PathBuf) -> Self {
        IngotPath(path.clone())
    }
}

impl From<Utf8PathBuf> for IngotPath {
    fn from(path: Utf8PathBuf) -> Self {
        IngotPath(path)
    }
}

impl Into<Utf8PathBuf> for IngotPath {
    fn into(self) -> Utf8PathBuf {
        self.0
    }
}

impl AsRef<Path> for IngotPath {
    fn as_ref(&self) -> &Path {
        self.0.as_std_path()
    }
}

impl AsBytes for IngotPath {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_str().as_bytes()
    }
}

unsafe impl NoPrefixesBytes for IngotPath {}
