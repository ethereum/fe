use fe_parser2::ast::{self};

use crate::{
    hir_def::{params::*, Body, IdentId, PathId, TypeId},
    span::FileId,
    HirDb,
};

impl GenericArgListId {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::GenericArgList) -> Self {
        let args = ast
            .into_iter()
            .map(|arg| GenericArg::from_ast(db, fid, arg))
            .collect();
        Self::new(db, args)
    }

    pub(crate) fn from_ast_opt(
        db: &dyn HirDb,
        fid: FileId,
        ast: Option<ast::GenericArgList>,
    ) -> Self {
        ast.map(|ast| Self::from_ast(db, fid, ast))
            .unwrap_or_else(|| Self::new(db, Vec::new()))
    }
}

impl GenericParamListId {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::GenericParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| GenericParam::from_ast(db, fid, param))
            .collect();
        Self::new(db, params)
    }

    pub(crate) fn from_ast_opt(
        db: &dyn HirDb,
        fid: FileId,
        ast: Option<ast::GenericParamList>,
    ) -> Self {
        ast.map(|ast| Self::from_ast(db, fid, ast))
            .unwrap_or_else(|| Self::new(db, Vec::new()))
    }
}

impl FnParamListId {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::FnParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| FnParam::from_ast(db, fid, param))
            .collect();
        Self::new(db, params)
    }
}

impl WhereClauseId {
    pub(crate) fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::WhereClause) -> Self {
        let predicates = ast
            .into_iter()
            .map(|pred| WherePredicate::from_ast(db, fid, pred))
            .collect();
        Self::new(db, predicates)
    }

    pub(crate) fn from_ast_opt(db: &dyn HirDb, fid: FileId, ast: Option<ast::WhereClause>) -> Self {
        ast.map(|ast| Self::from_ast(db, fid, ast))
            .unwrap_or_else(|| Self::new(db, Vec::new()))
    }
}

impl TypeGenericParam {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::TypeGenericParam) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::from_ast(db, fid, bound))
                    .collect()
            })
            .unwrap_or_default();

        Self { name, bounds }
    }
}

impl ConstGenericParam {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::ConstGenericParam) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let ty = TypeId::maybe_from_ast(db, fid, ast.ty());
        Self { name, ty }
    }
}

impl GenericArg {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::GenericArg) -> Self {
        match ast.kind() {
            ast::GenericArgKind::Type(type_param) => {
                TypeGenericArg::from_ast(db, fid, type_param).into()
            }
            ast::GenericArgKind::Const(const_param) => {
                ConstGenericArg::from_ast(db, fid, const_param).into()
            }
        }
    }
}

impl TypeGenericArg {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::TypeGenericArg) -> Self {
        let ty = TypeId::maybe_from_ast(db, fid, ast.ty());
        Self { ty }
    }
}

impl ConstGenericArg {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::ConstGenericArg) -> Self {
        let body = if let Some(expr) = ast.expr() {
            Some(Body::nameless_from_ast(db, fid, expr))
        } else {
            None
        }
        .into();

        Self { body }
    }
}

impl GenericParam {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::GenericParam) -> Self {
        match ast.kind() {
            ast::GenericParamKind::Type(type_param) => {
                TypeGenericParam::from_ast(db, fid, type_param).into()
            }
            ast::GenericParamKind::Const(const_param) => {
                ConstGenericParam::from_ast(db, fid, const_param).into()
            }
        }
    }
}

impl FnParam {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::FnParam) -> Self {
        let is_mut = ast.mut_token().is_some();
        let label = ast.label().map(|ast| FnParamLabel::from_ast(db, ast));
        let name = ast.name().map(|ast| FnParamName::from_ast(db, ast)).into();
        let ty = TypeId::maybe_from_ast(db, fid, ast.ty());

        Self {
            is_mut,
            label,
            name,
            ty,
        }
    }
}

impl WherePredicate {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::WherePredicate) -> Self {
        let ty = TypeId::maybe_from_ast(db, fid, ast.ty());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::from_ast(db, fid, bound))
                    .collect()
            })
            .unwrap_or_default();
        Self { ty, bounds }
    }
}

impl TypeBound {
    fn from_ast(db: &dyn HirDb, fid: FileId, ast: ast::TypeBound) -> Self {
        let path = ast.path().map(|ast| PathId::from_ast(db, ast)).into();
        let generic_args = ast
            .generic_args()
            .map(|args| GenericArgListId::from_ast(db, fid, args));
        Self { path, generic_args }
    }
}

impl FnParamName {
    fn from_ast(db: &dyn HirDb, ast: ast::FnParamName) -> Self {
        match ast {
            ast::FnParamName::Ident(name) => {
                FnParamName::Ident(IdentId::from_token(db, name.into()))
            }
            ast::FnParamName::SelfParam(_) => FnParamName::Self_,
            ast::FnParamName::Underscore(_) => FnParamName::Underscore,
        }
    }
}

impl FnParamLabel {
    fn from_ast(db: &dyn HirDb, ast: ast::FnParamLabel) -> Self {
        match ast {
            ast::FnParamLabel::Ident(name) => FnParamLabel::Ident(IdentId::from_token(db, name)),
            ast::FnParamLabel::Underscore(_) => FnParamLabel::Underscore,
        }
    }
}
