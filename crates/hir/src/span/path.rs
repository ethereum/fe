use parser::ast::{self, prelude::*};

use super::define_lazy_span_item;

define_lazy_span_item!(
    LazyPathSpan,
    ast::Path,
    @idx {
        (segment, LazyPathSegmentSpan),
    }
);

define_lazy_span_item!(LazyPathSegmentSpan);
