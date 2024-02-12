use parser::ast;

use crate::{
    hir_def::{Body, StmtId},
    span::types::LazyTySpan,
    SpannedHirDb,
};

use super::{
    body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, ResolvedOrigin, SpanTransitionChain},
};

define_lazy_span_node!(LazyStmtSpan, ast::Stmt,);
impl LazyStmtSpan {
    pub fn new(body: Body, stmt: StmtId) -> Self {
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
        (ty, type_annotation, LazyTySpan),
    }
);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct StmtRoot {
    stmt: StmtId,
    pub(crate) body: Body,
}

impl ChainInitiator for StmtRoot {
    fn init(&self, db: &dyn SpannedHirDb) -> ResolvedOrigin {
        let source_map = body_source_map(db, self.body);
        let origin = source_map.stmt_map.node_to_source(self.stmt);
        let top_mod = self.body.top_mod(db.as_hir_db());
        ResolvedOrigin::resolve(db, top_mod, origin)
    }
}

#[cfg(test)]
mod tests {
}
