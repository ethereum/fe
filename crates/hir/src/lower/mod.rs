use fe_parser2::SyntaxToken;

use crate::{
    hir_def::{IdentId, MaybeInvalid},
    HirDb,
};

mod attr;
mod body;
mod params;
mod path;
mod types;

impl IdentId {
    fn from_token(db: &dyn HirDb, token: SyntaxToken) -> Self {
        Self::new(db, token.text().to_string())
    }

    fn maybe_from_token(db: &dyn HirDb, token: Option<SyntaxToken>) -> MaybeInvalid<Self> {
        token.map(|token| Self::from_token(db, token)).into()
    }
}

impl<T> MaybeInvalid<T> {
    fn invalid() -> Self {
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
