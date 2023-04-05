use parser::ast;

use crate::span::path::LazyPathSpan;

use super::{define_lazy_span_item, types::LazyTypeSpan};

define_lazy_span_item!(
    LazyFnParamListSpan,
    ast::FnParamList,
    @idx {
        (param, LazyFnParamSpan),
    }
);

define_lazy_span_item!(
    LazyGenericParamListSpan,
    ast::GenericParamList,
    @idx {
        (param, LazyGenericParamSpan),
    }
);

define_lazy_span_item!(
    LazyGenericArgListSpan,
    ast::GenericArgList,
    @idx {
        (param, LazyGenericArgParamSpan),
    }

);
define_lazy_span_item!(
    LazyWhereClauseSpan,
    ast::WhereClause,
    @idx {
        (predicate, LazyWherePredicateSpan),
    }

);

define_lazy_span_item!(
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

define_lazy_span_item!(LazyFnParamLabelSpan);
define_lazy_span_item!(LazyFnParamNameSpan);
define_lazy_span_item!(LazyGenericParamSpan);
define_lazy_span_item!(LazyGenericArgParamSpan);

define_lazy_span_item!(
    LazyWherePredicateSpan,
    ast::WherePredicate,
    @node {
        (ty, ty, LazyTypeSpan),
        (bounds, bounds, LazyTypeBoundListSpan),
    }
);

define_lazy_span_item! {
    LazyTypeBoundListSpan,
    ast::TypeBoundList,
    @idx {
        (bound, LazyTypeBoundSpan),
    }
}

define_lazy_span_item!(
    LazyTypeBoundSpan,
    ast::TypeBound,
    @node {
        (path, path, LazyPathSpan),
        (generic_args, generic_args, LazyGenericArgListSpan),
    }
);
