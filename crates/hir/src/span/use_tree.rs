use parser::ast;

use super::define_lazy_span_node;

define_lazy_span_node!(
    LazyUseTreeSpan,
    ast::UseTree,
    @node {
        (path, path, LazyUsePathSpan),
        (subtree, children, LazySubUseTreeSpan),
        (alias, alias, LazyUseTreeAliasSpan),
    }
);

define_lazy_span_node!(
    LazyUsePathSpan,
    ast::UsePath,
    @idx {
        (segment, LazyUsePathSegmentSpan),
    }

);

define_lazy_span_node!(LazyUsePathSegmentSpan);

define_lazy_span_node!(
    LazySubUseTreeSpan,
    ast::UseTreeList,
    @idx {
        (tree, LazyUseTreeSpan),
    }
);

define_lazy_span_node!(
    LazyUseTreeAliasSpan,
    ast::UseTreeAlias,
    @token {
        (alias_name, ident),
    }
);
