use common::InputFile;
use parser::ast::{self, prelude::*};

use crate::{
    hir_def::{
        item::*, AttrListId, Body, FnParamListId, GenericParamListId, IdentId, TraitRef, TypeId,
        UseTreeId, WhereClauseId,
    },
    span::HirOrigin,
    HirDb,
};

impl Fn {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Fn,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Fn(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let params = ast
            .params()
            .map(|params| FnParamListId::from_ast(db, file, params))
            .into();
        let ret_ty = ast.ret_ty().map(|ty| TypeId::from_ast(db, file, ty));
        let modifier = ItemModifier::from_ast(db, ast.modifier());
        let body = ast.body().map(|body| {
            Body::item_body_from_ast(
                db,
                file,
                id.clone(),
                ast::Expr::cast(body.syntax().clone()).unwrap(),
            )
        });
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
            name,
            attributes,
            generic_params,
            where_clause,
            params,
            ret_ty,
            modifier,
            body,
            origin,
        )
    }
}

impl Struct {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Struct,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Struct(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let fields = RecordFieldListId::from_ast_opt(db, file, ast.fields());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
            name,
            attributes,
            is_pub,
            generic_params,
            where_clause,
            fields,
            origin,
        )
    }
}

impl Contract {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Contract,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Contract(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let fields = RecordFieldListId::from_ast_opt(db, file, ast.fields());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, id, name, attributes, is_pub, fields, origin)
    }
}

impl Enum {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Enum,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Enum(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let variants = EnumVariantListId::from_ast_opt(db, file, ast.variants());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
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
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::TypeAlias,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.alias());
        let id = TrackedItemId::TypeAlias(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
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
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Impl,
    ) -> Self {
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let id = TrackedItemId::Impl(ty).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, id, ty, attributes, generic_params, where_clause, origin)
    }
}

impl Trait {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Trait,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Trait(name).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let is_pub = ItemModifier::from_ast(db, ast.modifier()).is_pub();
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
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
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::ImplTrait,
    ) -> Self {
        let trait_ref = TraitRef::maybe_from_ast(db, file, ast.trait_ref());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let id = TrackedItemId::ImplTrait(trait_ref, ty).join_opt(parent_id);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let generic_params = GenericParamListId::from_ast_opt(db, file, ast.generic_params());
        let where_clause = WhereClauseId::from_ast_opt(db, file, ast.where_clause());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(
            db,
            id,
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
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Const,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Const(name).join_opt(parent_id);
        let body = ast
            .value()
            .map(|ast| Body::item_body_from_ast(db, file, id.clone(), ast))
            .into();

        let origin = HirOrigin::raw(file, &ast);
        Self::new(db, id, name, body, origin)
    }
}

impl Use {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent_id: Option<TrackedItemId>,
        ast: ast::Use,
    ) -> Self {
        let tree = UseTreeId::maybe_from_ast(db, ast.use_tree());
        let id = TrackedItemId::Use(tree).join_opt(parent_id);

        let origin = HirOrigin::raw(file, &ast);
        Self::new(db, id, tree, origin)
    }
}

impl ExternFn {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        file: InputFile,
        parent: Option<TrackedItemId>,
        ast: ast::Fn,
    ) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let id = TrackedItemId::Extern.join_opt(parent);

        let attributes = AttrListId::from_ast_opt(db, ast.attr_list());
        let params = ast
            .params()
            .map(|params| FnParamListId::from_ast(db, file, params))
            .into();
        let ret_ty = ast.ret_ty().map(|ty| TypeId::from_ast(db, file, ty));
        let modifier = ItemModifier::from_ast(db, ast.modifier());
        let origin = HirOrigin::raw(file, &ast);

        Self::new(db, id, name, attributes, params, ret_ty, modifier, origin)
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
    fn from_ast(db: &dyn HirDb, file: InputFile, ast: ast::RecordFieldDefList) -> Self {
        let fields = ast
            .into_iter()
            .map(|field| RecordField::from_ast(db, file, field))
            .collect();
        Self::new(db, fields)
    }

    fn from_ast_opt(db: &dyn HirDb, file: InputFile, ast: Option<ast::RecordFieldDefList>) -> Self {
        ast.map(|ast| Self::from_ast(db, file, ast))
            .unwrap_or(Self::new(db, Vec::new()))
    }
}

impl RecordField {
    fn from_ast(db: &dyn HirDb, file: InputFile, ast: ast::RecordFieldDef) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let ty = TypeId::maybe_from_ast(db, file, ast.ty());
        let is_pub = ast.pub_kw().is_some();

        Self { name, ty, is_pub }
    }
}

impl EnumVariantListId {
    fn from_ast(db: &dyn HirDb, file: InputFile, ast: ast::EnumVariantDefList) -> Self {
        let variants = ast
            .into_iter()
            .map(|variant| EnumVariant::from_ast(db, file, variant))
            .collect();
        Self::new(db, variants)
    }

    fn from_ast_opt(db: &dyn HirDb, file: InputFile, ast: Option<ast::EnumVariantDefList>) -> Self {
        ast.map(|ast| Self::from_ast(db, file, ast))
            .unwrap_or(Self::new(db, Vec::new()))
    }
}

impl EnumVariant {
    fn from_ast(db: &dyn HirDb, file: InputFile, ast: ast::EnumVariantDef) -> Self {
        let name = IdentId::maybe_from_token(db, ast.name());
        let ty = ast.ty().map(|ty| TypeId::from_ast(db, file, ty));

        Self { name, ty }
    }
}
