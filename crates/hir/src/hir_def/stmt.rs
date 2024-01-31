use cranelift_entity::entity_impl;

use super::{Body, ExprId, Partial, PatId, TypeId};
use crate::{span::stmt::LazyStmtSpan, HirDb};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    /// The `let` statement. The first `PatId` is the pattern for binding, the
    /// second `Option<TypeId>` is the type annotation, and the third
    /// `Option<ExprId>` is the expression for initialization.
    Let(PatId, Option<TypeId>, Option<ExprId>),
    /// The first `PatId` is the pattern for binding which can be used in the
    /// for-loop body.
    ///
    /// The second `ExprId` is the iterable expression.
    ///
    /// The third `ExprId` is the for-loop body.
    For(PatId, ExprId, ExprId),

    /// The first `ExprId` is the condition of the while-loop.
    /// The second `ExprId` is the body of the while-loop.
    While(ExprId, ExprId),
    Continue,
    Break,
    Return(Option<ExprId>),
    Expr(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(u32);
entity_impl!(StmtId);

impl StmtId {
    pub fn lazy_span(self, body: Body) -> LazyStmtSpan {
        LazyStmtSpan::new(body, self)
    }

    pub fn data(self, db: &dyn HirDb, body: Body) -> &Partial<Stmt> {
        &body.stmts(db)[self]
    }
}
