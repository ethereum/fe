use fe_parser2::ast;

use crate::{hir_def::Body, span::FileId, HirDb};

impl Body {
    pub(crate) fn from_ast_expr(db: &dyn HirDb, fid: FileId, ast: ast::Expr) -> Self {
        todo!()
    }

    pub(crate) fn from_ast_block(db: &dyn HirDb, fid: FileId, ast: ast::BlockExpr) -> Self {
        todo!()
    }
}
