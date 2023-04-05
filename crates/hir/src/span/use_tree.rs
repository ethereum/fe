use parser::ast;

use super::define_lazy_span_item;

define_lazy_span_item!(
    LazyUseTreeSpan,
    ast::UseTree,
    @node {
        (path, path, LazyUsePathSpan),
        (subtree, children, LazySubUseTreeSpan),
        (alias, alias, LazyUseTreeAliasSpan),
    }
);

define_lazy_span_item!(
    LazyUsePathSpan,
    ast::UsePath,
    @idx {
        (segment, LazyUsePathSegmentSpan),
    }

);

define_lazy_span_item!(LazyUsePathSegmentSpan);

define_lazy_span_item!(
    LazySubUseTreeSpan,
    ast::UseTreeList,
    @idx {
        (segment, LazyUseTreeSpan),
    }
);

define_lazy_span_item!(LazyUseTreeAliasSpan);
