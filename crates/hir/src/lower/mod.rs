use fe_parser2::SyntaxToken;

use crate::{hir_def::IdentId, HirDb};

mod body;
mod params;
mod path;
mod types;

impl IdentId {
    fn from_token(db: &dyn HirDb, token: Option<SyntaxToken>) -> Self {
        if let Some(token) = token {
            Self::new(db, token.text().to_string())
        } else {
            Self::invalid(db)
        }
    }

    fn invalid(db: &dyn HirDb) -> Self {
        Self::new(db, String::new())
    }
}
