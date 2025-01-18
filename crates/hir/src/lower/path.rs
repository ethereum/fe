use crate::hir_def::{GenericArgListId, IdentId, Partial, PathId};
use parser::ast::{self, GenericArgsOwner};

use super::FileLowerCtxt;

impl<'db> PathId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Path) -> Self {
        let db = ctxt.db();

        let mut path: Option<Self> = None;
        for seg in ast.into_iter() {
            let ident = match seg.kind() {
                Some(ast::PathSegmentKind::Ingot(_)) => Some(IdentId::make_ingot(db)),
                Some(ast::PathSegmentKind::Super(_)) => Some(IdentId::make_super(db)),
                Some(ast::PathSegmentKind::SelfTy(_)) => Some(IdentId::make_self_ty(db)),
                Some(ast::PathSegmentKind::Self_(_)) => Some(IdentId::make_self(db)),
                Some(ast::PathSegmentKind::Ident(ident)) => Some(IdentId::lower_token(ctxt, ident)),
                None => None,
            }
            .into();

            let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());

            path = path
                .map(|p| p.push(db, ident, generic_args))
                .or_else(|| Some(Self::new(db, ident, generic_args, None)))
        }

        path.expect("ast::Path must contain at least 1 segment")
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Path>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}
