use parser::ast;

use super::define_lazy_span_node;

define_lazy_span_node!(
    LazyPathSpan,
    ast::Path,
    @idx {
        (segment, LazyPathSegmentSpan),
    }
);

define_lazy_span_node!(LazyPathSegmentSpan);
