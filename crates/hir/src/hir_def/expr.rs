use cranelift_entity::entity_impl;

use crate::{span::expr::LazyExprSpan, HirDb};

use super::{Body, GenericArgListId, IdentId, IntegerId, LitKind, Partial, PatId, PathId, StmtId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(LitKind),
    Block(Vec<StmtId>),
    /// The first `ExprId` is the lhs, the second is the rhs.
    ///
    /// and a `BinOp`.
    Bin(ExprId, ExprId, Partial<BinOp>),
    Un(ExprId, Partial<UnOp>),
    /// The first `ExprId` is the callee, the second is the arguments.
    Call(ExprId, GenericArgListId, Vec<CallArg>),
    /// The first `ExprId` is the method receiver, the second is the method
    /// name, the third is the arguments.
    MethodCall(ExprId, Partial<IdentId>, GenericArgListId, Vec<CallArg>),
    Path(Partial<PathId>),
    /// The record construction expression.
    /// The fist `PathId` is the record type, the second is the record fields.
    RecordInit(Partial<PathId>, Vec<Field>),
    Field(ExprId, Partial<FieldIndex>),
    Tuple(Vec<ExprId>),
    /// The first `ExprId` is the indexed expression, the second is the index.
    Index(ExprId, ExprId),
    Array(Vec<ExprId>),

    /// The size of the rep should be the body instead of expression, because it
    /// should be resolved as a constant expression.
    ArrayRep(ExprId, Partial<Body>),

    /// The first `ExprId` is the condition, the second is the then branch, the
    /// third is the else branch.
    /// In case `else if`, the third is the lowered into `If` expression.
    If(ExprId, ExprId, Option<ExprId>),

    /// The first `ExprId` is the scrutinee, the second is the arms.
    Match(ExprId, Partial<Vec<MatchArm>>),

    /// The `Assign` Expression. The first `ExprId` is the destination of the assignment,
    /// and the second `ExprId` is the rhs value of the binding.
    Assign(ExprId, ExprId),

    AugAssign(ExprId, ExprId, ArithBinOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(u32);
entity_impl!(ExprId);

impl ExprId {
    pub fn lazy_span(self, body: Body) -> LazyExprSpan {
        LazyExprSpan::new(body, self)
    }

    pub fn data(self, db: &dyn HirDb, body: Body) -> &Partial<Expr> {
        &body.exprs(db)[self]
    }
}

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
pub struct MatchArm {
    pub pat: PatId,
    pub body: ExprId,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallArg {
    pub label: Option<IdentId>,
    pub expr: ExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Field {
    pub label: Option<IdentId>,
    pub expr: ExprId,
}
