use parser::ast::{self, prelude::*};

use crate::{
    hir_def::{
        item::*, AttrListId, Body, FuncParamListId, GenericParamListId, IdentId, TraitRefId,
        TupleTypeId, TypeId, WhereClauseId,
    },
    span::HirOrigin,
};

use super::FileLowerCtxt;

pub(crate) fn lower_module_items(ctxt: &mut FileLowerCtxt<'_>, items: ast::ItemList) {
    for item in items {
        ItemKind::lower_ast(ctxt, item);
    }
}

impl ItemKind {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Item) {
        let Some(kind) = ast.kind() else {
            return;
        };

        match kind {
            ast::ItemKind::Mod(mod_) => {
                Mod::lower_ast(ctxt, mod_);
            }
            ast::ItemKind::Func(fn_) => {
                Func::lower_ast(ctxt, fn_, false);
            }
            ast::ItemKind::Struct(struct_) => {
                Struct::lower_ast(ctxt, struct_);
            }
            ast::ItemKind::Contract(contract) => {
                Contract::lower_ast(ctxt, contract);
            }
            ast::ItemKind::Enum(enum_) => {
                Enum::lower_ast(ctxt, enum_);
            }
            ast::ItemKind::TypeAlias(alias) => {
                TypeAlias::lower_ast(ctxt, alias);
            }
            ast::ItemKind::Impl(impl_) => {
                Impl::lower_ast(ctxt, impl_);
            }
            ast::ItemKind::Trait(trait_) => {
                Trait::lower_ast(ctxt, trait_);
            }
            ast::ItemKind::ImplTrait(impl_trait) => {
                ImplTrait::lower_ast(ctxt, impl_trait);
            }
            ast::ItemKind::Const(const_) => {
                Const::lower_ast(ctxt, const_);
            }
            ast::ItemKind::Use(use_) => {
                Use::lower_ast(ctxt, use_);
            }
            ast::ItemKind::Extern(extern_) => {
                if let Some(extern_block) = extern_.extern_block() {
                    for fn_ in extern_block {
                        Func::lower_ast(ctxt, fn_, true);
                    }
                }
            }
        }
    }
}

impl Mod {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Mod) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Mod(name));
        ctxt.enter_item_scope(id.clone(), true);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        if let Some(items) = ast.items() {
            lower_module_items(ctxt, items);
        }

        let origin = HirOrigin::raw(&ast);
        let mod_ = Self::new(ctxt.db(), id, name, attributes, vis, ctxt.top_mod(), origin);
        ctxt.leave_item_scope(mod_)
    }
}

impl Func {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Func, is_extern: bool) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Func(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let params = ast
            .params()
            .map(|params| FuncParamListId::lower_ast(ctxt, params))
            .into();
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let modifier = ItemModifier::lower_ast(ast.modifier());
        let body = ast
            .body()
            .map(|body| Body::lower_ast(ctxt, ast::Expr::cast(body.syntax().clone()).unwrap()));
        let origin = HirOrigin::raw(&ast);

        let fn_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            generic_params,
            where_clause,
            params,
            ret_ty,
            modifier,
            body,
            is_extern,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(fn_)
    }
}

impl Struct {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Struct) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Struct(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let fields = FieldDefListId::lower_ast_opt(ctxt, ast.fields());
        let origin = HirOrigin::raw(&ast);

        let struct_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            where_clause,
            fields,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(struct_)
    }
}

impl Contract {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Contract) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Contract(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let fields = FieldDefListId::lower_ast_opt(ctxt, ast.fields());
        let origin = HirOrigin::raw(&ast);

        let contract = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            fields,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(contract)
    }
}

impl Enum {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Enum) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Enum(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let variants = VariantDefListId::lower_ast_opt(ctxt, ast.variants());
        let origin = HirOrigin::raw(&ast);

        let enum_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            where_clause,
            variants,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(enum_)
    }
}

impl TypeAlias {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TypeAlias) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.alias());
        let id = ctxt.joined_id(TrackedItemId::TypeAlias(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        let alias = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            ty,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(alias)
    }
}

impl Impl {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Impl) -> Self {
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemId::Impl(ty));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                Func::lower_ast(ctxt, impl_item, false);
            }
        }

        let impl_ = Self::new(
            ctxt.db(),
            id,
            ty,
            attributes,
            generic_params,
            where_clause,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(impl_)
    }
}

impl Trait {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Trait) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Trait(name));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let super_traits = if let Some(super_traits) = ast.super_trait_list() {
            super_traits
                .into_iter()
                .map(|trait_ref| TraitRefId::lower_ast(ctxt, trait_ref))
                .collect()
        } else {
            vec![]
        };
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                Func::lower_ast(ctxt, impl_item, false);
            }
        }

        let trait_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            super_traits,
            where_clause,
            ctxt.top_mod(),
            origin,
        );

        ctxt.leave_item_scope(trait_)
    }
}

impl ImplTrait {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::ImplTrait) -> Self {
        let trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemId::ImplTrait(trait_ref, ty));
        ctxt.enter_item_scope(id.clone(), false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                Func::lower_ast(ctxt, impl_item, false);
            }
        }

        let impl_trait = Self::new(
            ctxt.db(),
            id,
            trait_ref,
            ty,
            attributes,
            generic_params,
            where_clause,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(impl_trait)
    }
}

impl Const {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Const) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemId::Const(name));
        ctxt.enter_item_scope(id.clone(), false);

        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let body = ast.value().map(|ast| Body::lower_ast(ctxt, ast)).into();
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let origin = HirOrigin::raw(&ast);

        let const_ = Self::new(ctxt.db(), id, name, ty, body, vis, ctxt.top_mod(), origin);
        ctxt.leave_item_scope(const_)
    }
}

impl ItemModifier {
    pub(super) fn lower_ast(ast: Option<ast::ItemModifier>) -> Self {
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

impl FieldDefListId {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::RecordFieldDefList) -> Self {
        let fields = ast
            .into_iter()
            .map(|field| FieldDef::lower_ast(ctxt, field))
            .collect();
        Self::new(ctxt.db(), fields)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::RecordFieldDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }
}

impl FieldDef {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::RecordFieldDef) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let vis = if ast.pub_kw().is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Self { name, ty, vis }
    }
}

impl VariantDefListId {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::VariantDefList) -> Self {
        let variants = ast
            .into_iter()
            .map(|variant| VariantDef::lower_ast(ctxt, variant))
            .collect();
        Self::new(ctxt.db(), variants)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::VariantDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }
}

impl VariantDef {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::VariantDef) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let kind = match ast.kind() {
            ast::VariantKind::Unit => VariantKind::Unit,
            ast::VariantKind::Tuple(t) => VariantKind::Tuple(TupleTypeId::lower_ast(ctxt, t)),
            ast::VariantKind::Record(r) => VariantKind::Record(FieldDefListId::lower_ast(ctxt, r)),
        };
        Self { name, kind }
    }
}
