use crate::hir_def::{attr::*, IdentId, StringId};
use parser::ast;

use super::FileLowerCtxt;

impl AttrListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::AttrList) -> Self {
        let attrs = ast
            .into_iter()
            .map(|attr| Attr::lower_ast(ctxt, attr))
            .collect();
        Self::new(ctxt.db, attrs)
    }

    pub(super) fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::AttrList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db, vec![]))
    }
}

impl Attr {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Attr) -> Self {
        match ast.kind() {
            ast::AttrKind::Normal(attr) => NormalAttr::lower_ast(ctxt, attr).into(),
            ast::AttrKind::DocComment(attr) => DocCommentAttr::lower_ast(ctxt, attr).into(),
        }
    }
}

impl NormalAttr {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::NormalAttr) -> Self {
        let name = IdentId::maybe_lower_token(ctxt, ast.name());
        let args = ast
            .args()
            .map(|args| {
                args.into_iter()
                    .map(|arg| AttrArg::lower_ast(ctxt, arg))
                    .collect()
            })
            .unwrap_or_default();

        Self { name, args }
    }
}

impl DocCommentAttr {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::DocCommentAttr) -> Self {
        let text = ast
            .doc()
            .map(|doc| doc.text()[3..].to_string())
            .unwrap_or_default();
        Self {
            text: StringId::new(ctxt.db, text),
        }
    }
}

impl AttrArg {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::AttrArg) -> Self {
        let key = IdentId::maybe_lower_token(ctxt, ast.key());
        let value = IdentId::maybe_lower_token(ctxt, ast.value());
        Self { key, value }
    }
}
