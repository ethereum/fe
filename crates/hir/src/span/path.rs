use parser::ast;

use super::{define_lazy_span_node, LazySpanAtom};

define_lazy_span_node!(
    LazyPathSpan,
    ast::Path,
    @idx {
        (segment, LazyPathSegmentSpan),
    }
);

define_lazy_span_node!(LazyPathSegmentSpan);
impl LazyPathSegmentSpan {
    pub fn into_atom(self) -> LazySpanAtom {
        LazySpanAtom(self.0)
    }
}
