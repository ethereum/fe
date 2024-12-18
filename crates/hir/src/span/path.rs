use parser::ast;

use super::{define_lazy_span_node, params::LazyGenericArgListSpan, LazySpanAtom};

define_lazy_span_node!(
    LazyPathSpan,
    ast::Path,
    @idx {
        (segment, LazyPathSegmentSpan),
    }
);

define_lazy_span_node!(
    LazyPathSegmentSpan,
    ast::PathSegment,
    @token {
        (ident, ident),
    }
    @node {
        (generic_args, generic_args, LazyGenericArgListSpan),
    }
);
impl<'db> LazyPathSegmentSpan<'db> {
    pub fn into_atom(self) -> LazySpanAtom<'db> {
        LazySpanAtom(self.0)
    }
}
