use common::InputFile;
use parser::{ast, SyntaxNode};

use crate::{
    hir_def::{Body, StmtId},
    span::types::LazyTypeSpan,
    SpannedHirDb,
};

use super::{
    body_ast, body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, SpanTransitionChain},
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct StmtRoot {
    stmt: StmtId,
    body: Body,
}

impl ChainInitiator for StmtRoot {
    fn init(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode) {
        let source_map = body_source_map(db, self.body);
        let stmt_source = source_map.stmt_map.node_to_source(self.stmt);
        let ptr = stmt_source
            .syntax_ptr()
            .unwrap_or_else(|| body_ast(db, self.body).syntax_ptr().unwrap());

        let (file, root_node) = self.body.top_mod(db.upcast()).init(db);
        let node = ptr.to_node(&root_node);
        (file, node)
    }
}
