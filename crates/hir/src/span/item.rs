use parser::{ast, ast::prelude::*, SyntaxNode};

use crate::hir_def::{
    Const, Contract, Enum, ExternFn, Fn, Impl, ImplTrait, Mod, Struct, TopLevelMod, Trait,
    TypeAlias, Use,
};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_item,
    params::{LazyFnParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    span_impl_nodes, span_impl_tokens,
    types::{LazyPathTypeSpan, LazyTypeSpan},
    use_tree::LazyUseTreeSpan,
    SpanTransitionChain,
};

define_lazy_span_item!(LazyTopLevelModSpan);
impl LazyTopLevelModSpan {
    pub fn new(top_mod: TopLevelMod) -> Self {
        Self(SpanTransitionChain::new(top_mod.into()))
    }
}

define_lazy_span_item!(LazyModSpan);
impl LazyModSpan {
    pub fn new(mod_: Mod) -> Self {
        Self(SpanTransitionChain::new(mod_.into()))
    }

    span_impl_tokens!(ast::Mod, (name, name));
    span_impl_nodes!(
        ast::Mod,
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
    );
}

define_lazy_span_item!(LazyFnSpan);
impl LazyFnSpan {
    pub fn new(fn_: Fn) -> Self {
        Self(SpanTransitionChain::new(fn_.into()))
    }

    span_impl_tokens!(ast::Fn, (name, name));
    span_impl_nodes!(
        ast::Fn,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    );
}

define_lazy_span_item!(LazyExternFnSpan);
impl LazyExternFnSpan {
    pub fn new(fn_: ExternFn) -> Self {
        Self(SpanTransitionChain::new(fn_.into()))
    }

    span_impl_tokens!(ast::Fn, (name, name));
    span_impl_nodes!(
        ast::Fn,
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    );
}

define_lazy_span_item!(LazyStructSpan);
impl LazyStructSpan {
    pub fn new(struct_: Struct) -> Self {
        Self(SpanTransitionChain::new(struct_.into()))
    }

    span_impl_tokens!(ast::Struct, (name, name));
    span_impl_nodes!(
        ast::Struct,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldListSpan),
    );
}

define_lazy_span_item!(LazyContractSpan);
impl LazyContractSpan {
    pub fn new(contract: Contract) -> Self {
        Self(SpanTransitionChain::new(contract.into()))
    }

    span_impl_tokens!(ast::Contract, (name, name));
    span_impl_nodes!(
        ast::Contract,
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldListSpan),
    );
}

define_lazy_span_item!(LazyEnumSpan);
impl LazyEnumSpan {
    pub fn new(enum_: Enum) -> Self {
        Self(SpanTransitionChain::new(enum_.into()))
    }

    span_impl_tokens!(ast::Enum, (name, name));
    span_impl_nodes!(
        ast::Enum,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (variants, variants, LazyEnumVariantListSpan),
    );
}

define_lazy_span_item!(LazyTypeAliasSpan);
impl LazyTypeAliasSpan {
    pub fn new(alias: TypeAlias) -> Self {
        Self(SpanTransitionChain::new(alias.into()))
    }

    span_impl_tokens!(ast::TypeAlias, (alias, alias));
    span_impl_nodes!(
        ast::TypeAlias,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (ty, ty, LazyTypeSpan)
    );
}

define_lazy_span_item!(LazyImplSpan);
impl LazyImplSpan {
    pub fn new(impl_: Impl) -> Self {
        Self(SpanTransitionChain::new(impl_.into()))
    }

    span_impl_nodes!(
        ast::Impl,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (target_ty, ty, LazyTypeSpan),
    );
}

define_lazy_span_item!(LazyTraitSpan);
impl LazyTraitSpan {
    pub fn new(trait_: Trait) -> Self {
        Self(SpanTransitionChain::new(trait_.into()))
    }

    span_impl_tokens!(ast::Trait, (name, name));
    span_impl_nodes!(
        ast::Trait,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
    );
}

define_lazy_span_item!(LazyImplTraitSpan);
impl LazyImplTraitSpan {
    pub fn new(impl_trait: ImplTrait) -> Self {
        Self(SpanTransitionChain::new(impl_trait.into()))
    }

    span_impl_nodes!(
        ast::ImplTrait,
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (trait_ref, trait_ref, LazyPathTypeSpan),
        (ty, ty, LazyTypeSpan),
    );
}

define_lazy_span_item!(LazyConstSpan);
impl LazyConstSpan {
    pub fn new(const_: Const) -> Self {
        Self(SpanTransitionChain::new(const_.into()))
    }

    span_impl_tokens!(ast::Const, (name, name));
    span_impl_nodes!(
        ast::Const,
        (attributes, attr_list, LazyAttrListSpan),
        (ty, ty, LazyTypeSpan),
    );
}

define_lazy_span_item!(LazyUseSpan);
impl LazyUseSpan {
    pub fn new(use_: Use) -> Self {
        Self(SpanTransitionChain::new(use_.into()))
    }

    span_impl_nodes!(
        ast::Use,
        (attributes, attr_list, LazyAttrListSpan),
        (use_tree, use_tree, LazyUseTreeSpan),
    );
}

define_lazy_span_item!(LazyRecordFieldListSpan);
impl LazyRecordFieldListSpan {
    pub fn field(&self, idx: usize) -> LazyRecordFieldSpan {
        let transition = move |node: SyntaxNode| {
            ast::RecordFieldList::cast(node)
                .and_then(|f| f.into_iter().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyRecordFieldSpan(self.0.push_state(std::sync::Arc::new(transition)))
    }
}

define_lazy_span_item!(LazyRecordFieldSpan);
impl LazyRecordFieldSpan {
    span_impl_tokens!(ast::RecordFieldDef, (pub_kw, pub_kw), (name, name),);
    span_impl_nodes!(ast::RecordFieldDef, (ty, ty, LazyTypeSpan));
}

define_lazy_span_item!(LazyEnumVariantListSpan);
impl LazyEnumVariantListSpan {
    pub fn field(&self, idx: usize) -> LazyEnumVariantSpan {
        let transition = move |node: SyntaxNode| {
            ast::EnumVariantDefList::cast(node)
                .and_then(|f| f.into_iter().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyEnumVariantSpan(self.0.push_state(std::sync::Arc::new(transition)))
    }
}

define_lazy_span_item!(LazyEnumVariantSpan);
impl LazyEnumVariantSpan {
    span_impl_tokens!(ast::EnumVariantDef, (name, name));
    span_impl_nodes!(ast::EnumVariantDef, (ty, ty, LazyTypeSpan));
}

define_lazy_span_item!(LazyItemModifierSpan);
impl LazyItemModifierSpan {
    span_impl_tokens!(ast::ItemModifier, (pub_kw, pub_kw), (unsafe_kw, unsafe_kw));
}
