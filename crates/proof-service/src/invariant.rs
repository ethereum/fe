use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Invariant {
    pub name: SmolStr,
    pub body: InvariantBody,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InvariantHeader {
    pub name: SmolStr,
    pub id: u64,
}

#[derive(Hash, Debug, Serialize, Deserialize)]
pub struct InvariantBody {
    pub args: Vec<SmolStr>,
    pub code: String,
}

impl Invariant {
    pub fn new(name: SmolStr, args: Vec<SmolStr>, code: String) -> Self {
        Self {
            name,
            body: InvariantBody { args, code },
        }
    }

    pub fn id(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.body.hash(&mut s);
        s.finish()
    }

    pub fn header(&self) -> InvariantHeader {
        InvariantHeader {
            name: self.name.clone(),
            id: self.id(),
        }
    }
}
