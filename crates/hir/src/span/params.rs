use parser::ast;

use crate::span::path::LazyPathSpan;

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
        (label, label, LazyFnParamLabelSpan),
        (name, name, LazyFnParamNameSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(LazyFnParamLabelSpan);
define_lazy_span_node!(LazyFnParamNameSpan);
define_lazy_span_node!(LazyGenericParamSpan);
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
