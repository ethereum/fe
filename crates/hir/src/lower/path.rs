use parser::{ast, SyntaxToken};

use crate::hir_def::{IdentId, Partial, PathId, PathSegment};

use super::FileLowerCtxt;

impl PathId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = if seg.is_self() {
                Some(PathSegment::Self_)
            } else if seg.is_self_ty() {
                Some(PathSegment::SelfTy)
            } else if let Some(ident) = seg.ident() {
                Some(PathSegment::Ident(IdentId::new(
                    ctxt.db,
                    ident.text().to_string(),
                )))
            } else {
                None
            }
            .into();
            segments.push(segment);
        }

        Self::new(ctxt.db, segments)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::Path>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }

    pub(super) fn from_ident(ctxt: &mut FileLowerCtxt<'_>, ast: SyntaxToken) -> Self {
        let ident_id = IdentId::new(ctxt.db, ast.text().to_string());
        let seg = vec![Partial::Present(PathSegment::Ident(ident_id))];
        Self::new(ctxt.db, seg)
    }
}
