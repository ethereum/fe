use parser::ast;

use crate::span::{path::LazyPathSpan, LazySpanAtom};

use super::{define_lazy_span_node, types::LazyTySpan};

define_lazy_span_node!(
    LazyFuncParamListSpan,
    ast::FuncParamList,
    @idx {
        (param, LazyFuncParamSpan),
    }
);

define_lazy_span_node!(
    LazyGenericParamListSpan,
    ast::GenericParamList,
    @idx {
        (param, LazyGenericParamSpan),
    }
);

define_lazy_span_node!(
    LazyGenericArgListSpan,
    ast::GenericArgList,
    @idx {
        (arg, LazyGenericArgSpan),
    }

);
define_lazy_span_node!(
    LazyWhereClauseSpan,
    ast::WhereClause,
    @token {
        (where_token, where_kw),
    }
    @idx {
        (predicate, LazyWherePredicateSpan),
    }
);

define_lazy_span_node!(
    LazyFuncParamSpan,
    ast::FuncParam,
    @token {
       (mut_kw, mut_token),
    }
    @node {
       (label, label, LazySpanAtom),
       (name, name, LazySpanAtom),
       (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(LazyGenericParamSpan, ast::GenericParam);
impl LazyGenericParamSpan {
    pub fn into_type_param(self) -> LazyTypeGenericParamSpan {
        LazyTypeGenericParamSpan(self.0)
    }

    pub fn into_const_param(self) -> LazyConstGenericParamSpan {
        LazyConstGenericParamSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyTypeGenericParamSpan,
    ast::TypeGenericParam,
    @token {
        (name, name),
    }
    @node {
        (bounds, bounds, LazyTypeBoundListSpan),
    }
);

define_lazy_span_node!(
    LazyConstGenericParamSpan,
    ast::ConstGenericParam,
    @token {
        (const_token, const_kw),
        (name, name),
    }
    @node {
        (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(LazyGenericArgSpan);
impl LazyGenericArgSpan {
    pub fn into_type_arg(self) -> LazyTypeGenericArgSpan {
        LazyTypeGenericArgSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyTypeGenericArgSpan,
    ast::TypeGenericArg,
    @node {
        (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(
    LazyWherePredicateSpan,
    ast::WherePredicate,
    @node {
        (ty, ty, LazyTySpan),
        (bounds, bounds, LazyTypeBoundListSpan),
    }
);

define_lazy_span_node! {
    LazyTypeBoundListSpan,
    ast::TypeBoundList,
    @idx {
        (bound, LazyTypeBoundSpan),
    }
}

define_lazy_span_node!(
    LazyTypeBoundSpan,
    ast::TypeBound,
    @node {
        (trait_bound, trait_bound, LazyTraitBoundSpan),
        (kind_bound, kind_bound, LazyKindBoundSpan),
    }
);

define_lazy_span_node!(
    LazyTraitBoundSpan,
    ast::TraitBound,
    @node {
        (path, path, LazyPathSpan),
        (generic_args, generic_args, LazyGenericArgListSpan),
    }
);

define_lazy_span_node!(
    LazyKindBoundSpan,
    ast::KindBound,
    @token {
        (arrow, arrow),
    }
    @node {
       (lhs, lhs, LazyKindBoundSpan),
       (rhs, rhs, LazyKindBoundSpan ),
    }
);
