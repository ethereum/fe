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
    pub fn is_self(&self, db: &dyn HirDb) -> bool {
        self.data(db) == "self"
    }
}

#[salsa::interned]
pub struct IntegerId {
    #[return_ref]
    pub data: BigUint,
}

#[salsa::interned]
pub struct StringId {
    /// The text of the string literal, without the quotes.
    #[return_ref]
    pub data: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LitKind {
    Int(IntegerId),
    String(StringId),
    Bool(bool),
}

/// This enum is used to represent a type that may be invalid in terms of the
/// syntax.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MaybeInvalid<T> {
    Valid(T),
    Invalid,
}

impl<T> MaybeInvalid<T> {
    pub(crate) fn valid(t: T) -> Self {
        Self::Valid(t)
    }

    pub(crate) fn invalid() -> Self {
        Self::Invalid
    }
}

impl<T> From<Option<T>> for MaybeInvalid<T> {
    fn from(value: Option<T>) -> Self {
        if let Some(value) = value {
            Self::Valid(value)
        } else {
            Self::Invalid
        }
    }
}
