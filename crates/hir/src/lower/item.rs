use fe_parser2::ast::{self, prelude::*};

use crate::{
    hir_def::{
        item::*, AttrListId, FnParamListId, GenericParamListId, IdentId, TraitRef, TypeId,
        UseTreeId, WhereClauseId,
    },
    input::File,
    span::HirOrigin,
    HirDb,
};

impl Fn {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Fn) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_paramas = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let params = ast
            .params()
            .map(|params| FnParamListId::from_ast(db, file, params))
            .into();
        let ret_ty = ast.ret_ty().map(|ty| TypeId::from_ast(db, file, ty));
        let modifier = ItemModifier::from_ast(db, ast.modifier());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            name,
            attributes,
            generic_paramas,
            where_clause,
            params,
            ret_ty,
            modifier,
            origin,
        )
    }
}

impl Struct {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Struct) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_paramas = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let fields = RecordFieldListId::from_ast_opt(db, file, ast.fields());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            name,
            attributes,
            is_pub,
            generic_paramas,
            where_clause,
            fields,
            origin,
        )
    }
}

impl Contract {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Contract) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let fields = RecordFieldListId::from_ast_opt(db, file, ast.fields());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, name, attributes, is_pub, fields, origin)
    }
}

impl Enum {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Enum) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let variants = EnumVariantListId::from_ast_opt(db, file, ast.variants());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            name,
            attributes,
            is_pub,
            generic_params,
            where_clause,
            variants,
            origin,
        )
    }
}

impl TypeAlias {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::TypeAlias) -> Self {
        let name = IdentId::maybe_from_token(db, ast.alias());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            name,
            attributes,
            is_pub,
            generic_params,
            where_clause,
            ty,
            origin,
        )
    }
}

impl Impl {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Impl) -> Self {
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, ty, attributes, generic_params, where_clause, origin)
    }
}

impl Trait {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Trait) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            name,
            attributes,
            is_pub,
            generic_params,
            where_clause,
            origin,
        )
    }
}

impl ImplTrait {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::ImplTrait) -> Self {
        let trait_ref = TraitRef::maybe_from_ast(db, file, ast.trait_ref());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            trait_ref,
            ty,
            attributes,
            generic_params,
            where_clause,
            origin,
        )
    }
}

impl Const {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Const) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());

        let origin = HirOrigin::raw(file, &ast);
        Self::new(db, name, origin)
    }
}

impl Use {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Use) -> Self {
        let tree = UseTreeId::maybe_from_ast(db, ast.use_tree());
        let origin = HirOrigin::raw(file, &ast);
        Self::new(db, tree, origin)
    }
}

impl Extern {
    pub(crate) fn from_ast(db: &dyn HirDb, file: File, ast: ast::Extern) -> Self {
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, origin)
    }
}

impl ItemModifier {
    fn from_ast(db: &dyn HirDb, ast: Option<ast::ItemModifier>) -> Self {
        let Some(ast) = ast else {
            return Self::None;
        };

        match (ast.pub_kw().is_some(), ast.unsafe_kw().is_some()) {
            (true, true) => Self::PubAndUnsafe,
            (true, false) => Self::Pub,
            (false, true) => Self::Unsafe,
            (false, false) => Self::None,
        }
    }
}

impl RecordFieldListId {
    fn from_ast(db: &dyn HirDb, file: File, ast: ast::RecordFieldDefList) -> Self {
        let fields = ast
            .into_iter()
            .map(|field| RecordField::from_ast(db, file, field))
            .collect();
        Self::new(db, fields)
    }

    fn from_ast_opt(db: &dyn HirDb, file: File, ast: Option<ast::RecordFieldDefList>) -> Self {
        ast.map(|ast| Self::from_ast(db, file, ast))
            .unwrap_or(Self::new(db, Vec::new()))
    }
}

impl RecordField {
    fn from_ast(db: &dyn HirDb, file: File, ast: ast::RecordFieldDef) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let is_pub = ast.pub_kw().is_some();

        Self { name, ty, is_pub }
    }
}

impl EnumVariantListId {
    fn from_ast(db: &dyn HirDb, file: File, ast: ast::EnumVariantDefList) -> Self {
        let variants = ast
            .into_iter()
            .map(|variant| EnumVariant::from_ast(db, file, variant))
            .collect();
        Self::new(db, variants)
    }

    fn from_ast_opt(db: &dyn HirDb, file: File, ast: Option<ast::EnumVariantDefList>) -> Self {
        ast.map(|ast| Self::from_ast(db, file, ast))
            .unwrap_or(Self::new(db, Vec::new()))
    }
}

impl EnumVariant {
    fn from_ast(db: &dyn HirDb, file: File, ast: ast::EnumVariantDef) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let ty = ast.ty().map(|ty| TypeId::from_ast(db, file, ty));

        Self { name, ty }
    }
}
