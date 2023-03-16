pub mod attr;
pub mod body;
pub mod expr;
pub mod item;
pub mod params;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod types;
pub mod use_tree;

pub use attr::*;
pub use body::*;
pub use expr::*;
pub use item::*;
use num_bigint::BigUint;
pub use params::*;
pub use pat::*;
pub use path::*;
pub use stmt::*;
pub use types::*;
pub use use_tree::*;

use crate::HirDb;

#[salsa::interned]
pub struct IdentId {
    data: String,
}
impl IdentId {
    pub fn is_invalid(self, db: &dyn HirDb) -> bool {
        self.data(db).is_empty()
    }

    pub fn is_self(&self, db: &dyn HirDb) -> bool {
        self.data(db) == "self"
    }
}

#[salsa::interned]
pub struct IntegerId {
    data: BigUint,
}

#[salsa::interned]
pub struct StringId {
    data: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LitKind {
    Int(IntegerId),
    String(StringId),
}
