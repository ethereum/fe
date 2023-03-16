use fe_parser2::ast;

use crate::{
    hir_def::{IdentId, PathId, PathSegment},
    HirDb,
};

impl PathId {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: Option<ast::Path>) -> Self {
        // If the path is None, we return a path with a single invalid segment.
        let Some(ast) = ast else {
            return Self::new(db, vec![PathSegment::Invalid])
        };

        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = if seg.is_self() {
                PathSegment::Self_
            } else if seg.is_self_ty() {
                PathSegment::SelfTy
            } else if let Some(ident) = seg.ident() {
                PathSegment::Ident(IdentId::new(db, ident.text().to_string()))
            } else {
                PathSegment::Invalid
            };
            segments.push(segment);
        }

        Self::new(db, segments)
    }
}
