use crate::hir_def::{attr::*, IdentId, StringId};
use parser::ast;

use crate::HirDb;

impl AttrListId {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::AttrList) -> Self {
        let attrs = ast
            .into_iter()
            .map(|attr| Attr::from_ast(db, attr))
            .collect();
        Self::new(db, attrs)
    }

    pub fn from_ast_opt(db: &dyn HirDb, ast: Option<ast::AttrList>) -> Self {
        ast.map(|ast| Self::from_ast(db, ast))
            .unwrap_or_else(|| Self::new(db, vec![]))
    }
}

impl Attr {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::Attr) -> Self {
        match ast.kind() {
            ast::AttrKind::Normal(attr) => NormalAttr::from_ast(db, attr).into(),
            ast::AttrKind::DocComment(attr) => DocCommentAttr::from_ast(db, attr).into(),
        }
    }
}

impl NormalAttr {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::NormalAttr) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let args = ast
            .args()
            .map(|args| {
                args.into_iter()
                    .map(|arg| AttrArg::from_ast(db, arg))
                    .collect()
            })
            .unwrap_or_default();

        Self { name, args }
    }
}

impl DocCommentAttr {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::DocCommentAttr) -> Self {
        let text = ast
            .doc()
            .map(|doc| doc.text()[3..].to_string())
            .unwrap_or_default();
        Self {
            text: StringId::new(db, text),
        }
    }
}

impl AttrArg {
    pub(crate) fn from_ast(db: &dyn HirDb, ast: ast::AttrArg) -> Self {
        let key = IdentId::maybe_from_token(db, ast.key());
        let value = IdentId::maybe_from_token(db, ast.value());
        Self { key, value }
    }
}
