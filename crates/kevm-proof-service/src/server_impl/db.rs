use std::{fmt::Display, fs, io::Write};

use indexmap::{indexmap, IndexMap};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Serialize, Deserialize)]
pub struct DbEntry {
    pub name: SmolStr,
    pub complete: bool,
}

impl DbEntry {
    pub fn new(name: SmolStr, complete: bool) -> Self {
        Self { name, complete }
    }
}

impl Display for DbEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "name: {0: <20} complete: {1: <5}",
            self.name, self.complete
        )
    }
}

pub struct Db {
    path: String,
    entries: IndexMap<u64, DbEntry>,
}

impl Display for &Db {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, entry) in self.entries.iter() {
            writeln!(f, "id: {0: <20} -> {1: <40}", id, entry)?
        }

        Ok(())
    }
}

impl Db {
    pub fn new(path: &str) -> Self {
        let content = fs::read(path).unwrap();

        let entries: IndexMap<u64, DbEntry> = if content.len() == 0 {
            indexmap! {}
        } else {
            serde_yaml::from_slice(&content).unwrap()
        };

        Db {
            path: path.to_string(),
            entries,
        }
    }

    pub fn write(&self) {
        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.path)
            .unwrap();
        file.write_all(serde_yaml::to_string(&self.entries).unwrap().as_bytes())
            .unwrap();
    }

    pub fn update(&mut self, id: u64, entry: DbEntry) {
        self.entries.insert(id, entry);
    }

    pub fn get(&self, id: u64) -> Option<&DbEntry> {
        self.entries.get(&id)
    }

    pub fn evict(&mut self, id: u64) {
        self.entries.remove(&id);
    }

    pub fn get_mut(&mut self, id: u64) -> Option<&mut DbEntry> {
        self.entries.get_mut(&id)
    }
}
