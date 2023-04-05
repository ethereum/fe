use common::InputFile;
use parser::{ast, SyntaxNode};

use crate::{
    hir_def::{Body, PatId},
    parse_file,
    span::path::LazyPathSpan,
};

use super::{
    db::SpannedHirDb,
    define_lazy_span_item,
    transition::{ChainRoot, SpanTransitionChain},
};

define_lazy_span_item!(LazyPatSpan, ast::Pat,);
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

define_lazy_span_item!(
    LazyPathPatSpan,
    ast::PathPat,
    @node {
        (path, path, LazyPathSpan),
    }
);

define_lazy_span_item!(
    LazyPathTuplePatSpan,
    ast::PathTuplePat,
    @node {
        (path, path, LazyPathSpan),
    }
);

define_lazy_span_item!(
    LazyRecordPatSpan,
    ast::RecordPat,
    @node {
        (path, path, LazyPathSpan),
        (field, fields, LazyRecordPatFieldListSpan),
    }
);

define_lazy_span_item!(
    LazyRecordPatFieldListSpan,
    ast::RecordPatFieldList,
    @idx {
        (field, LazyRecordPatSpan),
    }
);

define_lazy_span_item!(
    LazyRecordPatFieldSpan,
    ast::RecordPatField,
    @token {
        (name, name),
    }
);

#[derive(Clone, Copy)]
struct PatRoot {
    pat: PatId,
    body: Body,
}

impl ChainRoot for PatRoot {
    fn root(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode) {
        let body_ast = db.body_ast(self.body);
        let file = body_ast.file;
        let source_map = db.body_source_map(self.body);
        let pat_source = source_map.pat_map.node_to_source(self.pat);
        let ptr = pat_source.syntax_ptr().unwrap();

        let root_node = SyntaxNode::new_root(parse_file(db.upcast(), file));
        let node = ptr.to_node(&root_node);
        (file, node)
    }
}
