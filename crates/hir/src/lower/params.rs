use fe_parser2::ast;

use crate::{hir_def::GenericArgListId, HirDb};

impl GenericArgListId {
    pub fn from_ast(db: &dyn HirDb, ast: ast::GenericArgList) -> Self {
        todo!()
    }
}
