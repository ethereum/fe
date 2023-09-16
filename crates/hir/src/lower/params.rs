use parser::ast::{self};

use crate::hir_def::{kw, params::*, Body, IdentId, Partial, PathId, TypeId};

use super::FileLowerCtxt;

impl GenericArgListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericArgList) -> Self {
        let args = ast
            .into_iter()
            .map(|arg| GenericArg::lower_ast(ctxt, arg))
            .collect();
        Self::new(ctxt.db(), args)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::GenericArgList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), Vec::new()))
    }
}

impl GenericParamListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::GenericParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| GenericParam::lower_ast(ctxt, param))
            .collect();
        Self::new(ctxt.db(), params)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::GenericParamList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), Vec::new()))
    }
}

impl FuncParamListId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FuncParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| FuncParam::lower_ast(ctxt, param))
            .collect();
        Self::new(ctxt.db(), params)
    }
}

impl WhereClauseId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::WhereClause) -> Self {
        let predicates = ast
            .into_iter()
            .map(|pred| WherePredicate::lower_ast(ctxt, pred))
            .collect();
        Self::new(ctxt.db(), predicates)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::WhereClause>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), Vec::new()))
    }
}

impl TypeGenericParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TypeGenericParam) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
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
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
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
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        Self { ty }
    }
}

impl ConstGenericArg {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::ConstGenericArg) -> Self {
        let body = ast
            .expr()
            .map(|expr| Body::lower_ast_nameless(ctxt, expr))
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

impl FuncParam {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FuncParam) -> Self {
        let is_mut = ast.mut_token().is_some();
        let label = ast.label().map(|ast| FuncParamLabel::lower_ast(ctxt, ast));
        let name = ast
            .name()
            .map(|ast| FuncParamName::lower_ast(ctxt, ast))
            .into();
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());

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
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
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
        if let Some(trait_bound) = ast.trait_bound() {
            Self::Trait(TraitBound::lower_ast(ctxt, trait_bound))
        } else {
            Self::Kind(KindBound::lower_ast_opt(ctxt, ast.kind_bound()))
        }
    }
}

impl TraitBound {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TraitBound) -> Self {
        let path = ast.path().map(|ast| PathId::lower_ast(ctxt, ast)).into();
        let generic_args = ast
            .generic_args()
            .map(|args| GenericArgListId::lower_ast(ctxt, args));
        Self { path, generic_args }
    }
}

impl KindBound {
    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::KindBound>) -> Partial<Self> {
        let Some(ast) = ast else {
            return Partial::Absent;
        };

        if let Some(abs) = ast.abs() {
            let lhs = KindBound::lower_ast_opt(ctxt, abs.lhs())
                .to_opt()
                .map(|kind| Box::new(kind))
                .into();

            let rhs = KindBound::lower_ast_opt(ctxt, abs.rhs())
                .to_opt()
                .map(|kind| Box::new(kind))
                .into();

            Partial::Present(KindBound::Abs(lhs, rhs))
        } else if let Some(_) = ast.mono() {
            Partial::Present(KindBound::Mono)
        } else {
            Partial::Absent
        }
    }
}

impl FuncParamName {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FuncParamName) -> Self {
        match ast {
            ast::FuncParamName::Ident(name) => {
                FuncParamName::Ident(IdentId::lower_token(ctxt, name))
            }
            ast::FuncParamName::SelfParam(_) => FuncParamName::Ident(kw::SELF),
            ast::FuncParamName::Underscore(_) => FuncParamName::Underscore,
        }
    }
}

impl FuncParamLabel {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::FuncParamLabel) -> Self {
        match ast {
            ast::FuncParamLabel::Ident(name) => {
                FuncParamLabel::Ident(IdentId::lower_token(ctxt, name))
            }
            ast::FuncParamLabel::Underscore(_) => FuncParamLabel::Underscore,
        }
    }
}
