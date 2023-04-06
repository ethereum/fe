use parser::ast;

use crate::hir_def::{
    Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
    Trait, TypeAlias, Use,
};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_node,
    params::{LazyFnParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    types::{LazyPathTypeSpan, LazyTypeSpan},
    use_tree::LazyUseTreeSpan,
};

define_lazy_span_node!(LazyTopLevelModSpan, ast::Root, new(TopLevelMod),);

define_lazy_span_node!(
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

define_lazy_span_node!(
    LazyFnSpan,
    ast::Fn,
    new(Func),
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

define_lazy_span_node!(
    LazyExternFnSpan,
    ast::Fn,
    new(ExternFunc),
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

define_lazy_span_node!(
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
        (fields, fields, LazyRecordFieldDefListSpan),
    }
);

define_lazy_span_node!(
    LazyContractSpan,
    ast::Contract,
    new(Contract),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldDefListSpan),
    }
);

define_lazy_span_node!(
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

define_lazy_span_node!(
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

define_lazy_span_node!(
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
define_lazy_span_node!(
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

define_lazy_span_node!(
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

define_lazy_span_node!(
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

define_lazy_span_node!(
    LazyUseSpan,
    ast::Use,
    new(Use),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (use_tree, use_tree, LazyUseTreeSpan),
    }
);

define_lazy_span_node!(LazyBodySpan, ast::Expr, new(Body),);

define_lazy_span_node!(
    LazyRecordFieldDefListSpan,
    ast::RecordFieldDefList,
    @idx {
        (field, LazyRecordFieldDefSpan),
    }
);

define_lazy_span_node!(
    LazyRecordFieldDefSpan,
    ast::RecordFieldDef,
    @token {
        (pub_kw, pub_kw),
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyEnumVariantListSpan,
    ast::EnumVariantDefList,
    @idx {
        (variant, LazyEnumVariantSpan),
    }
);

define_lazy_span_node!(
    LazyEnumVariantSpan,
    ast::EnumVariantDef,
    @token {
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyItemModifierSpan,
    ast::ItemModifier,
    @token {
        (pub_kw, pub_kw),
        (unsafe_kw, unsafe_kw),
    }
);
