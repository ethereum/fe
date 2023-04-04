use parser::{
    ast::{self, prelude::*},
    SyntaxNode,
};

use super::define_lazy_span_item;

define_lazy_span_item!(LazyPathSpan);
impl LazyPathSpan {
    pub fn segment(&self, idx: usize) -> LazyPathSegmentSpan {
        let transition = move |node: SyntaxNode| {
            ast::Path::cast(node)
                .and_then(|f| f.into_iter().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyPathSegmentSpan(self.0.push_state(std::sync::Arc::new(transition)))
    }
}

define_lazy_span_item!(LazyPathSegmentSpan);
