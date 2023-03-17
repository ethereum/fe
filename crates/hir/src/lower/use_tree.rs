use fe_parser2::ast;

use crate::{
    hir_def::{use_tree::*, IdentId, MaybeInvalid},
    HirDb,
};

impl UseTreeId {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::UseTree) -> Self {
        let path = if let Some(path) = ast.path() {
            path.into_iter()
                .map(|ast| UsePathSegment::maybe_from_ast(db, ast))
                .collect()
        } else {
            vec![]
        };
        let subtree = if let Some(children) = ast.children() {
            children
                .into_iter()
                .map(|ast| UseTreeId::from_ast(db, ast))
                .collect()
        } else {
            vec![]
        };
        let alias = ast.alias().map(|ast| UseTreeAlias::maybe_from_ast(db, ast));

        Self::new(db, path, subtree, alias)
    }

    pub(crate) fn maybe_from_ast(db: &dyn HirDb, ast: Option<ast::UseTree>) -> MaybeInvalid<Self> {
        ast.map(|ast| Self::from_ast(db, ast)).into()
    }
}

impl UsePathSegment {
    pub(crate) fn maybe_from_ast(db: &dyn HirDb, ast: ast::UsePathSegment) -> MaybeInvalid<Self> {
        ast.kind()
            .map(|kind| match kind {
                ast::UsePathSegmentKind::Ident(ident) => {
                    Self::Ident(IdentId::from_token(db, ident))
                }
                ast::UsePathSegmentKind::SelfPath(_) => Self::SelfPath,
                ast::UsePathSegmentKind::Glob(_) => Self::Glob,
            })
            .into()
    }
}

impl UseTreeAlias {
    pub(crate) fn maybe_from_ast(db: &dyn HirDb, ast: ast::UseTreeAlias) -> MaybeInvalid<Self> {
        if let Some(ident) = ast.ident() {
            Some(Self::Ident(IdentId::from_token(db, ident)))
        } else if ast.underscore().is_some() {
            Some(Self::Underscore)
        } else {
            None
        }
        .into()
    }
}
