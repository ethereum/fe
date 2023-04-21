use parser::ast;

use crate::{
    hir_def::{Body, StmtId},
    span::types::LazyTypeSpan,
    SpannedHirDb,
};

use super::{
    body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, ResolvedOrigin, SpanTransitionChain},
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
    fn init(&self, db: &dyn SpannedHirDb) -> ResolvedOrigin {
        let source_map = body_source_map(db, self.body);
        let origin = source_map.stmt_map.node_to_source(self.stmt);
        let top_mod = self.body.top_mod(db.upcast());
        ResolvedOrigin::resolve(db, top_mod, origin)
    }
}

#[cfg(test)]
mod tests {
    use crate::{hir_def::Body, test_db::TestDb};
    use common::Upcast;

    #[test]
    fn aug_assign() {
        let mut db = TestDb::default();

        let text = r#" {
            fn foo() {
                let mut x = 0
                x += 1
            }
        }"#;

        let body: Body = db.expect_item::<Body>(text);
        let top_mod = body.top_mod(db.upcast());
        for (i, stmt) in body.stmts(db.upcast()).keys().enumerate() {
            match i {
                0 => {
                    let span = stmt.lazy_span(body);
                    assert_eq!("let mut x = 0", db.text_at(top_mod, &span));
                }
                1 => {
                    let span = stmt.lazy_span(body);
                    assert_eq!("x += 1", db.text_at(top_mod, &span));
                }
                _ => unreachable!(),
            }
        }
    }
}
