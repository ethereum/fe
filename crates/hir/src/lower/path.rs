use fe_parser2::ast;

use crate::{
    hir_def::{IdentId, MaybeInvalid, PathId, PathSegment},
    HirDb,
};

impl PathId {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = if seg.is_self() {
                MaybeInvalid::Valid(PathSegment::Self_)
            } else if seg.is_self_ty() {
                MaybeInvalid::Valid(PathSegment::SelfTy)
            } else if let Some(ident) = seg.ident() {
                MaybeInvalid::Valid(PathSegment::Ident(IdentId::new(
                    db,
                    ident.text().to_string(),
                )))
            } else {
                MaybeInvalid::invalid()
            };
            segments.push(segment);
        }

        Self::new(db, segments)
    }

    pub(crate) fn maybe_from_ast(db: &dyn HirDb, ast: Option<ast::Path>) -> MaybeInvalid<Self> {
        ast.map(|ast| Self::from_ast(db, ast)).into()
    }
}
