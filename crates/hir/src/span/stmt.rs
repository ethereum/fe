use common::InputFile;
use parser::{ast, SyntaxNode};

use crate::{
    hir_def::{Body, StmtId},
    parse_file,
    span::types::LazyTypeSpan,
};

use super::{
    db::SpannedHirDb,
    define_lazy_span_node,
    transition::{ChainRoot, SpanTransitionChain},
};

define_lazy_span_node!(LazyStmtSpan, ast::Stmt,);
impl LazyStmtSpan {
    pub fn new(stmt: StmtId, body: Body) -> Self {
        let root = StmtRoot { stmt, body };
        Self(SpanTransitionChain::new(root))
    }

    pub fn into_let_stmt(self) -> LazyLetStmtSpan {
        LazyLetStmtSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyLetStmtSpan,
    ast::LetStmt,
    @node {
        (ty, type_annotation, LazyTypeSpan),
    }
);

#[derive(Clone, Copy)]
struct StmtRoot {
    stmt: StmtId,
    body: Body,
}

impl ChainRoot for StmtRoot {
    fn root(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode) {
        let body_ast = db.body_ast(self.body);
        let file = body_ast.file;
        let source_map = db.body_source_map(self.body);
        let pat_source = source_map.stmt_map.node_to_source(self.stmt);
        let ptr = pat_source
            .syntax_ptr()
            .unwrap_or_else(|| body_ast.syntax_ptr().unwrap());

        let root_node = SyntaxNode::new_root(parse_file(db.upcast(), file));
        let node = ptr.to_node(&root_node);
        (file, node)
    }
}
