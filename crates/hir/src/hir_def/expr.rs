use cranelift_entity::entity_impl;

use super::{Body, IdentId, IntegerId, LitKind, MaybeInvalid, PatId, PathId, StmtId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(LitKind),
    BlockExpr(Vec<StmtId>),
    /// The first `ExprId` is the lhs, the second is the rhs.
    ///
    /// **NOTE:** The `AugAssign` statement is desugared to a `Assign` statement
    /// and a `BinOp`.
    Bin(ExprId, ExprId, MaybeInvalid<BinOp>),
    Un(ExprId, UnOp),
    /// The first `ExprId` is the callee, the second is the arguments.
    Call(ExprId, Vec<ExprId>),
    /// The first `ExprId` is the method receiver, the second is the method
    /// name, the third is the arguments.
    MethodCall(ExprId, IdentId, Vec<ExprId>),
    Path(MaybeInvalid<PathId>),
    /// The record construction expression.
    /// The fist `PathId` is the record type, the second is the record fields.
    Record(PathId, Vec<(IdentId, ExprId)>),
    Field(ExprId, FieldIndex),
    Tuple(Vec<ExprId>),
    /// The first `ExprId` is the indexed expression, the second is the index.
    Index(ExprId, ExprId),
    ArrayExpr(Vec<ExprId>),

    /// The size of the rep should be the body instead of expression, becuase it
    /// should be resolved as a contatnt expressison.
    ArrayRepExpr(ExprId, Body),

    /// The first `ExprId` is the condition, the second is the then branch, the
    /// third is the else branch.
    If(ExprId, ExprId, Option<ExprId>),

    /// The first `ExprId` is the scrutinee, the second is the arms.
    Match(ExprId, Vec<MatchArm>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(u32);
entity_impl!(ExprId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldIndex {
    /// The field is indexed by its name.
    /// `field.foo`.
    Ident(IdentId),
    /// The field is indexed by its integer.
    /// `field.0`.
    Index(IntegerId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatchArm {
    /// The first `Part` is the pattern, the second is
    /// the arm body.
    MatchArm(PatId, ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub enum BinOp {
    Arith(ArithBinOp),
    Comp(CompBinOp),
    Logical(LogicalBinOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithBinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `**`
    Pow,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`  
    BitXor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompBinOp {
    /// `==`
    Eq,
    /// `!=`
    NotEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicalBinOp {
    /// `&&`
    And,
    /// `||`
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `!`
    Not,
    /// `~`
    BitNot,
}
