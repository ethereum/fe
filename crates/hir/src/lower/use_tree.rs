use parser::ast;

use crate::hir_def::{use_tree::*, IdentId, Partial};

use super::FileLowerCtxt;

impl UseTreeId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::UseTree) -> Self {
        let path = if let Some(path) = ast.path() {
            path.into_iter()
                .map(|ast| UsePathSegment::lower_ast_partial(ctxt, ast))
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
            .map(|ast| UseAlias::lower_ast_partial(ctxt, ast));

        Self::new(ctxt.db(), path, subtree, alias)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::UseTree>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

impl UsePathSegment {
    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: ast::UsePathSegment,
    ) -> Partial<Self> {
        ast.kind()
            .map(|kind| match kind {
                ast::UsePathSegmentKind::Ingot(_) => Self::Ident(IdentId::ingot_kw(ctxt.db())),
                ast::UsePathSegmentKind::Super(_) => Self::Ident(IdentId::super_kw(ctxt.db())),
                ast::UsePathSegmentKind::Ident(ident) => {
                    Self::Ident(IdentId::lower_token(ctxt, ident))
                }
                ast::UsePathSegmentKind::Self_(_) => Self::Ident(IdentId::self_kw(ctxt.db())),
                ast::UsePathSegmentKind::Glob(_) => Self::Glob,
            })
            .into()
    }
}

impl UseAlias {
    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: ast::UseTreeAlias,
    ) -> Partial<Self> {
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
