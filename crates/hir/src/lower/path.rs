use parser::ast;

use super::FileLowerCtxt;
use crate::hir_def::{IdentId, Partial, PathId};

impl<'db> PathId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Path) -> Self {
        let mut segments = Vec::new();
        let db = ctxt.db();

        for seg in ast.into_iter() {
            let segment = match seg.kind() {
                Some(ast::PathSegmentKind::Ingot(_)) => Some(IdentId::make_ingot(db)),
                Some(ast::PathSegmentKind::Super(_)) => Some(IdentId::make_super(db)),
                Some(ast::PathSegmentKind::SelfTy(_)) => Some(IdentId::make_self_ty(db)),
                Some(ast::PathSegmentKind::Self_(_)) => Some(IdentId::make_self(db)),
                Some(ast::PathSegmentKind::Ident(ident)) => Some(IdentId::lower_token(ctxt, ident)),
                None => None,
            }
            .into();
            segments.push(segment);
        }

        Self::new(db, segments)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Path>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}
