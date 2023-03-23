use parser::ast;

use crate::hir_def::{use_tree::*, IdentId, MaybeInvalid};

use super::FileLowerCtxt;

impl UseTreeId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::UseTree) -> Self {
        let path = if let Some(path) = ast.path() {
            path.into_iter()
                .map(|ast| UsePathSegment::maybe_lower_ast(ctxt, ast))
                .collect()
        } else {
            vec![]
        };
        let subtree = if let Some(children) = ast.children() {
            children
                .into_iter()
                .map(|ast| UseTreeId::lower_ast(ctxt, ast))
                .collect()
        } else {
            vec![]
        };
        let alias = ast
            .alias()
            .map(|ast| UseTreeAlias::maybe_lower_ast(ctxt, ast));

        Self::new(ctxt.db, path, subtree, alias)
    }

    pub(super) fn maybe_lower_ast(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::UseTree>,
    ) -> MaybeInvalid<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

impl UsePathSegment {
    pub(super) fn maybe_lower_ast(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: ast::UsePathSegment,
    ) -> MaybeInvalid<Self> {
        ast.kind()
            .map(|kind| match kind {
                ast::UsePathSegmentKind::Ident(ident) => {
                    Self::Ident(IdentId::lower_token(ctxt, ident))
                }
                ast::UsePathSegmentKind::SelfPath(_) => Self::SelfPath,
                ast::UsePathSegmentKind::Glob(_) => Self::Glob,
            })
            .into()
    }
}

impl UseTreeAlias {
    pub(super) fn maybe_lower_ast(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: ast::UseTreeAlias,
    ) -> MaybeInvalid<Self> {
        if let Some(ident) = ast.ident() {
            Some(Self::Ident(IdentId::lower_token(ctxt, ident)))
        } else if ast.underscore().is_some() {
            Some(Self::Underscore)
        } else {
            None
        }
        .into()
    }
}
