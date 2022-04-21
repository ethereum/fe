use parser::{ast, SyntaxToken};

use crate::hir_def::{IdentId, Partial, PathId};

use super::FileLowerCtxt;

impl PathId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = match seg.kind() {
                Some(ast::PathSegmentKind::Ingot(_)) => Some(IdentId::ingot_kw(ctxt.db())),
                Some(ast::PathSegmentKind::Super(_)) => Some(IdentId::super_kw(ctxt.db())),
                Some(ast::PathSegmentKind::SelfTy(_)) => Some(IdentId::self_ty_kw(ctxt.db())),
                Some(ast::PathSegmentKind::Self_(_)) => Some(IdentId::self_kw(ctxt.db())),
                Some(ast::PathSegmentKind::Ident(ident)) => Some(IdentId::lower_token(ctxt, ident)),
                None => None,
            }
            .into();
            segments.push(segment);
        }

        Self::new(ctxt.db(), segments)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::Path>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }

    pub(super) fn from_ident(ctxt: &mut FileLowerCtxt<'_>, ast: SyntaxToken) -> Self {
        let ident_id = IdentId::new(ctxt.db(), ast.text().to_string());
        let seg = vec![Partial::Present(ident_id)];
        Self::new(ctxt.db(), seg)
    }
}
