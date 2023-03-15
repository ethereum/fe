use cranelift_entity::entity_impl;

use super::{ExprId, PatId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    /// The `let` statement. The first `PatId` is the pattern for binding, the
    /// second `Option<TypeId>` is the type annotation, and the third
    /// `Option<ExprId>` is the expression for initialization.
    Let(PatId, Option<TypeId>, Option<ExprId>),
    /// The `Assign` statement. The first `PatId` is the pattern for binding,
    /// and the second `ExprId` is the rhs value of the binding.
    Assign(PatId, ExprId),
    /// The first `PatId` is the pattern for binding which can be used in the
    /// for-loop body.
    ///
    /// The second `ExprId` is the iterator expression.
    ///
    /// The third `ExprId` is the for-loop body.
    For(PatId, ExprId, ExprId),
    Continue,
    Break,
    Return(Option<ExprId>),
    Expr(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(u32);
entity_impl!(StmtId);
