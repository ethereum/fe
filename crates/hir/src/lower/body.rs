use fe_parser2::ast;

use crate::{hir_def::Body, span::FileId, HirDb};

impl Body {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::Expr, fid: FileId) -> Self {
        todo!()
    }

    pub(crate) fn invalid(db: &dyn HirDb, fid: FileId) -> Self {
        todo!()
    }
}
