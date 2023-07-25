use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct SymbolicTest {
    pub name: SmolStr,
    pub body: SymbolicTestBody,
}

#[derive(Hash, Debug, Serialize, Deserialize)]
pub struct SymbolicTestBody {
    pub args: Vec<SmolStr>,
    pub code: String,
}

impl SymbolicTest {
    pub fn new(name: SmolStr, args: Vec<SmolStr>, code: String) -> Self {
        Self {
            name,
            body: SymbolicTestBody { args, code },
        }
    }

    pub fn id(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.body.hash(&mut s);
        s.finish()
    }
}
