use fe_parser2::{ast, SyntaxToken};

use crate::{
    hir_def::{IdentId, MaybeInvalid, PathId, PathSegment},
    HirDb,
};

impl PathId {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = if seg.is_self() {
                Some(PathSegment::Self_)
            } else if seg.is_self_ty() {
                Some(PathSegment::SelfTy)
            } else if let Some(ident) = seg.ident() {
                Some(PathSegment::Ident(IdentId::new(
                    db,
                    ident.text().to_string(),
                )))
            } else {
                None
            }
            .into();
            segments.push(segment);
        }

        Self::new(db, segments)
    }

    pub(crate) fn maybe_from_ast(db: &dyn HirDb, ast: Option<ast::Path>) -> MaybeInvalid<Self> {
        ast.map(|ast| Self::from_ast(db, ast)).into()
    }

    pub(super) fn from_ident(db: &dyn HirDb, ast: SyntaxToken) -> Self {
        let ident_id = IdentId::new(db, ast.text().to_string());
        let seg = vec![MaybeInvalid::Valid(PathSegment::Ident(ident_id))];
        Self::new(db, seg)
    }
}
