use parser::{
    ast::{self, prelude::*},
    SyntaxNode,
};

use super::{define_lazy_span_item, span_impl_nodes};

define_lazy_span_item!(LazyUseTreeSpan);
impl LazyUseTreeSpan {
    span_impl_nodes!(
        ast::UseTree,
        (path, path, LazyUsePathSpan),
        (subtree, children, LazySubUseTreeSpan),
        (alias, alias, LazyUseTreeAliasSpan),
    );
}

define_lazy_span_item!(LazyUsePathSpan);
impl LazyUsePathSpan {
    pub fn segment(&self, idx: usize) -> LazyUsePathSegmentSpan {
        let transition = move |node: SyntaxNode| {
            ast::UsePath::cast(node)
                .and_then(|f| f.into_iter().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyUsePathSegmentSpan(self.0.push_state(std::sync::Arc::new(transition)))
    }
}

define_lazy_span_item!(LazyUsePathSegmentSpan);

define_lazy_span_item!(LazySubUseTreeSpan);
impl LazySubUseTreeSpan {
    pub fn subtree(&self, idx: usize) -> LazyUseTreeSpan {
        let transition = move |node: SyntaxNode| {
            ast::UseTreeList::cast(node)
                .and_then(|f| f.into_iter().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyUseTreeSpan(self.0.push_state(std::sync::Arc::new(transition)))
    }
}

define_lazy_span_item!(LazyUseTreeAliasSpan);
