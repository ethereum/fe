use parser::{ast, SyntaxToken};

use crate::hir_def::{kw, IdentId, Partial, PathId};

use super::FileLowerCtxt;

impl PathId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        for seg in ast.into_iter() {
            let segment = match seg.kind() {
                Some(ast::PathSegmentKind::Ingot(_)) => Some(kw::INGOT),
                Some(ast::PathSegmentKind::Super(_)) => Some(kw::SUPER),
                Some(ast::PathSegmentKind::SelfTy(_)) => Some(kw::SELF_TY),
                Some(ast::PathSegmentKind::Self_(_)) => Some(kw::SELF),
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
