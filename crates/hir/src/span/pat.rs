use common::InputFile;
use parser::{ast, SyntaxNode};

use crate::{
    hir_def::{Body, PatId},
    span::path::LazyPathSpan,
    SpannedHirDb,
};

use super::{
    body_ast, body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, SpanTransitionChain},
};

define_lazy_span_node!(LazyPatSpan, ast::Pat,);
impl LazyPatSpan {
    pub fn new(pat: PatId, body: Body) -> Self {
        let root = PatRoot { pat, body };
        Self(SpanTransitionChain::new(root))
    }

    pub fn into_path_pat(self) -> LazyPathPatSpan {
        LazyPathPatSpan(self.0)
    }

    pub fn into_path_tuple_pat(self) -> LazyPathPatSpan {
        LazyPathPatSpan(self.0)
    }

    pub fn into_record_pat(self) -> LazyRecordPatSpan {
        LazyRecordPatSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyPathPatSpan,
    ast::PathPat,
    @node {
        (path, path, LazyPathSpan),
    }
);

define_lazy_span_node!(
    LazyPathTuplePatSpan,
    ast::PathTuplePat,
    @node {
        (path, path, LazyPathSpan),
    }
);

define_lazy_span_node!(
    LazyRecordPatSpan,
    ast::RecordPat,
    @node {
        (path, path, LazyPathSpan),
        (field, fields, LazyRecordPatFieldListSpan),
    }
);

define_lazy_span_node!(
    LazyRecordPatFieldListSpan,
    ast::RecordPatFieldList,
    @idx {
        (field, LazyRecordPatSpan),
    }
);

define_lazy_span_node!(
    LazyRecordPatFieldSpan,
    ast::RecordPatField,
    @token {
        (name, name),
    }
);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct PatRoot {
    pat: PatId,
    body: Body,
}

impl ChainInitiator for PatRoot {
    fn init(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode) {
        let source_map = body_source_map(db, self.body);
        let pat_source = source_map.pat_map.node_to_source(self.pat);
        let ptr = pat_source
            .syntax_ptr()
            .unwrap_or_else(|| body_ast(db, self.body).syntax_ptr().unwrap());

        let (file, root_node) = self.body.top_mod(db.upcast()).init(db);
        let node = ptr.to_node(&root_node);
        (file, node)
    }
}
