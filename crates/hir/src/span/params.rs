use parser::ast;

use crate::span::{path::LazyPathSpan, LazySpanAtom};

use super::{define_lazy_span_node, types::LazyTypeSpan};

define_lazy_span_node!(
    LazyFnParamListSpan,
    ast::FnParamList,
    @idx {
        (param, LazyFnParamSpan),
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
        (param, LazyGenericArgParamSpan),
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
    LazyFnParamSpan,
    ast::FnParam,
    @token {
       (mut_kw, mut_token),
    }
    @node {
       (label, label, LazySpanAtom),
       (name, name, LazySpanAtom),
       (ty, ty, LazyTypeSpan),
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
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(LazyGenericArgParamSpan);

define_lazy_span_node!(
    LazyWherePredicateSpan,
    ast::WherePredicate,
    @node {
        (ty, ty, LazyTypeSpan),
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
        (path, path, LazyPathSpan),
        (generic_args, generic_args, LazyGenericArgListSpan),
    }
);
