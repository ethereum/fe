use parser::ast::{self};

use crate::hir_def::{params::*, Body, IdentId, PathId, TypeId};

use super::FileLowerCtxt;

impl GenericArgListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericArgList) -> Self {
        let args = ast
            .into_iter()
            .map(|arg| GenericArg::lower_ast(ctxt, arg))
            .collect();
        Self::new(ctxt.db, args)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::GenericArgList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db, Vec::new()))
    }
}

impl GenericParamListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| GenericParam::lower_ast(ctxt, param))
            .collect();
        Self::new(ctxt.db, params)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::GenericParamList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db, Vec::new()))
    }
}

impl FnParamListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FnParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| FnParam::lower_ast(ctxt, param))
            .collect();
        Self::new(ctxt.db, params)
    }
}

impl WhereClauseId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::WhereClause) -> Self {
        let predicates = ast
            .into_iter()
            .map(|pred| WherePredicate::lower_ast(ctxt, pred))
            .collect();
        Self::new(ctxt.db, predicates)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::WhereClause>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db, Vec::new()))
    }
}

impl TypeGenericParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TypeGenericParam) -> Self {
        let name = IdentId::maybe_lower_token(ctxt, ast.name());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();

        Self { name, bounds }
    }
}

impl ConstGenericParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::ConstGenericParam) -> Self {
        let name = IdentId::maybe_lower_token(ctxt, ast.name());
        let ty = TypeId::maybe_lower_ast(ctxt, ast.ty());
        Self { name, ty }
    }
}

impl GenericArg {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericArg) -> Self {
        match ast.kind() {
            ast::GenericArgKind::Type(type_param) => {
                TypeGenericArg::lower_ast(ctxt, type_param).into()
            }
            ast::GenericArgKind::Const(const_param) => {
                ConstGenericArg::lower_ast(ctxt, const_param).into()
            }
        }
    }
}

impl TypeGenericArg {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TypeGenericArg) -> Self {
        let ty = TypeId::maybe_lower_ast(ctxt, ast.ty());
        Self { ty }
    }
}

impl ConstGenericArg {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::ConstGenericArg) -> Self {
        let body = if let Some(expr) = ast.expr() {
            Some(Body::lower_ast_nameless(ctxt, expr))
        } else {
            None
        }
        .into();

        Self { body }
    }
}

impl GenericParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericParam) -> Self {
        match ast.kind() {
            ast::GenericParamKind::Type(type_param) => {
                TypeGenericParam::lower_ast(ctxt, type_param).into()
            }
            ast::GenericParamKind::Const(const_param) => {
                ConstGenericParam::lower_ast(ctxt, const_param).into()
            }
        }
    }
}

impl FnParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FnParam) -> Self {
        let is_mut = ast.mut_token().is_some();
        let label = ast.label().map(|ast| FnParamLabel::lower_ast(ctxt, ast));
        let name = ast
            .name()
            .map(|ast| FnParamName::lower_ast(ctxt, ast))
            .into();
        let ty = TypeId::maybe_lower_ast(ctxt, ast.ty());

        Self {
            is_mut,
            label,
            name,
            ty,
        }
    }
}

impl WherePredicate {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::WherePredicate) -> Self {
        let ty = TypeId::maybe_lower_ast(ctxt, ast.ty());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();
        Self { ty, bounds }
    }
}

impl TypeBound {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TypeBound) -> Self {
        let path = ast.path().map(|ast| PathId::lower_ast(ctxt, ast)).into();
        let generic_args = ast
            .generic_args()
            .map(|args| GenericArgListId::lower_ast(ctxt, args));
        Self { path, generic_args }
    }
}

impl FnParamName {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FnParamName) -> Self {
        match ast {
            ast::FnParamName::Ident(name) => {
                FnParamName::Ident(IdentId::lower_token(ctxt, name.into()))
            }
            ast::FnParamName::SelfParam(_) => FnParamName::Self_,
            ast::FnParamName::Underscore(_) => FnParamName::Underscore,
        }
    }
}

impl FnParamLabel {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FnParamLabel) -> Self {
        match ast {
            ast::FnParamLabel::Ident(name) => FnParamLabel::Ident(IdentId::lower_token(ctxt, name)),
            ast::FnParamLabel::Underscore(_) => FnParamLabel::Underscore,
        }
    }
}
