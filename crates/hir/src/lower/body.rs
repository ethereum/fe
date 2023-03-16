use fe_parser2::ast;

use crate::{hir_def::Body, span::FileId, HirDb};

impl Body {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::Expr) -> Self {
        todo!()
    }

    pub(crate) fn invalid(db: &dyn HirDb, fid: FileId) -> Self {
        todo!()
    }
}
