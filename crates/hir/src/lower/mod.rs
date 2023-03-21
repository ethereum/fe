use fe_parser2::{ast, SyntaxToken};
use num_bigint::BigUint;
use num_traits::Num;

use crate::{
    hir_def::{IdentId, IntegerId, LitKind, MaybeInvalid, StringId},
    HirDb,
};

mod attr;
mod body;
mod expr;
mod item;
mod params;
mod pat;
mod path;
mod stmt;
mod types;
mod use_tree;

impl IdentId {
    fn from_token(db: &dyn HirDb, token: SyntaxToken) -> Self {
        Self::new(db, token.text().to_string())
    }

    fn maybe_from_token(db: &dyn HirDb, token: Option<SyntaxToken>) -> MaybeInvalid<Self> {
        token.map(|token| Self::from_token(db, token)).into()
    }
}

impl LitKind {
    pub(super) fn from_ast(db: &dyn HirDb, ast: ast::Lit) -> Self {
        match ast.kind() {
            ast::LitKind::Int(int) => Self::Int(IntegerId::from_ast(db, int)),
            ast::LitKind::String(string) => {
                let text = string.token().text();
                Self::String(StringId::new(db, text[1..text.len() - 1].to_string()))
            }
            ast::LitKind::Bool(bool) => match bool.token().text() {
                "true" => Self::Bool(true),
                "false" => Self::Bool(false),
                _ => unreachable!(),
            },
        }
    }
}

impl IntegerId {
    pub(super) fn from_ast(db: &dyn HirDb, ast: ast::LitInt) -> Self {
        let text = ast.token().text();
        // Parser ensures that the text is valid pair with a radix and a number.
        if text.len() < 2 {
            return Self::new(db, BigUint::from_str_radix(&text, 10).unwrap());
        }

        let int = match &text[0..2] {
            "0x" | "0X" => BigUint::from_str_radix(&text[2..], 16).unwrap(),
            "0o" | "0O" => BigUint::from_str_radix(&text[2..], 8).unwrap(),
            "0b" | "0B" => BigUint::from_str_radix(&text[2..], 2).unwrap(),
            _ => BigUint::from_str_radix(&text, 10).unwrap(),
        };

        Self::new(db, int)
    }
}
