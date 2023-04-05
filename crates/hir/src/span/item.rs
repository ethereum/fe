use parser::{ast, ast::prelude::*, SyntaxNode};

use crate::hir_def::{
    Const, Contract, Enum, ExternFn, Fn, Impl, ImplTrait, Mod, Struct, TopLevelMod, Trait,
    TypeAlias, Use,
};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_item,
    params::{LazyFnParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    types::{LazyPathTypeSpan, LazyTypeSpan},
    use_tree::LazyUseTreeSpan,
};

define_lazy_span_item!(LazyTopLevelModSpan, ast::Root, new(TopLevelMod),);

define_lazy_span_item!(
    LazyModSpan,
    ast::Mod,
    new(Mod),
    @token
    {
        (name, name),
    }
    @node
    {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
    }
);

define_lazy_span_item!(
    LazyFnSpan,
    ast::Fn,
    new(Fn),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyExternFnSpan,
    ast::Fn,
    new(ExternFn),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyStructSpan,
    ast::Struct,
    new(Struct),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldListSpan),
    }
);

define_lazy_span_item!(
    LazyContractSpan,
    ast::Contract,
    new(Contract),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldListSpan),
    }
);

define_lazy_span_item!(
    LazyEnumSpan,
    ast::Enum,
    new(Enum),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (variants, variants, LazyEnumVariantListSpan),
    }
);

define_lazy_span_item!(
    LazyTypeAliasSpan,
    ast::TypeAlias,
    new(TypeAlias),
    @token {
        (alias, alias),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyImplSpan,
    ast::Impl,
    new(Impl),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (target_ty, ty, LazyTypeSpan),
    }
);
define_lazy_span_item!(
    LazyTraitSpan,
    ast::Trait,
    new(Trait),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
    }
);

define_lazy_span_item!(
    LazyImplTraitSpan,
    ast::ImplTrait,
    new(ImplTrait),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (trait_ref, trait_ref, LazyPathTypeSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyConstSpan,
    ast::Const,
    new(Const),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyUseSpan,
    ast::Use,
    new(Use),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (use_tree, use_tree, LazyUseTreeSpan),
    }
);

define_lazy_span_item!(
    LazyRecordFieldListSpan,
    ast::RecordFieldList,
    @idx {
        (field, LazyRecordFieldListSpan),
    }
);

define_lazy_span_item!(
    LazyRecordFieldSpan,
    ast::RecordFieldDef,
    @token {
        (pub_kw, pub_kw),
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyEnumVariantListSpan,
    ast::EnumVariantDefList,
    @idx {
        (variant, LazyEnumVariantSpan),
    }
);

define_lazy_span_item!(
    LazyEnumVariantSpan,
    ast::EnumVariantDef,
    @token {
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_item!(
    LazyItemModifierSpan,
    ast::ItemModifier,
    @token {
        (pub_kw, pub_kw),
        (unsafe_kw, unsafe_kw),
    }
);
