use id_arena::Id;

use super::{basic_block::BasicBlockId, function::FunctionId, value::ValueId, SourceInfo, TypeId};

pub type InstId = Id<Inst>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    pub kind: InstKind,
    pub source: SourceInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    /// This is not a real instruction, just used to tag a position where a
    /// local is declared.
    Declare {
        local: ValueId,
    },

    Assign {
        lhs: ValueId,
        rhs: ValueId,
    },

    /// Unary instruction.
    Unary {
        op: UnOp,
        value: ValueId,
    },

    /// Binary instruction.
    Binary {
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
    },

    Cast {
        value: ValueId,
        to: TypeId,
    },

    /// Constructs aggregate value, i.e. struct, tuple and array.
    AggregateConstruct {
        ty: TypeId,
        args: Vec<ValueId>,
    },

    /// Access to aggregate fields or elements.
    /// # Example
    ///
    /// ```fe
    /// struct Foo:
    ///     x: i32
    ///     y: Array<i32, 8>
    /// ```
    /// `foo.y` is lowered into `AggregateAccess(foo, [1])' for example.
    AggregateAccess {
        value: ValueId,
        index: ValueId,
    },

    MapAccess {
        value: ValueId,
        key: ValueId,
    },

    Call {
        func: FunctionId,
        args: Vec<ValueId>,
        call_type: CallType,
    },

    /// Unconditional jump instruction.
    Jump {
        dest: BasicBlockId,
    },

    /// Conditional branching instruction.
    Branch {
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
    },

    Revert {
        arg: ValueId,
    },

    Emit {
        arg: ValueId,
    },

    Return {
        arg: ValueId,
    },

    Intrinsic {
        op: IntrinsicOp,
        args: Vec<ValueId>,
    },
}

impl Inst {
    pub fn new(kind: InstKind, source: SourceInfo) -> Self {
        Self { kind, source }
    }

    pub fn unary(op: UnOp, value: ValueId, source: SourceInfo) -> Self {
        let kind = InstKind::Unary { op, value };
        Self::new(kind, source)
    }

    pub fn binary(op: BinOp, lhs: ValueId, rhs: ValueId, source: SourceInfo) -> Self {
        let kind = InstKind::Binary { op, lhs, rhs };
        Self::new(kind, source)
    }

    pub fn intrinsic(op: IntrinsicOp, args: Vec<ValueId>, source: SourceInfo) -> Self {
        let kind = InstKind::Intrinsic { op, args };
        Self::new(kind, source)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// `not` operator for logical inversion.
    Not,
    /// `-` operator for negation.
    Neg,
    /// `~` operator for bitwise inversion.
    Inv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Shl,
    Shr,
    BitOr,
    BitXor,
    BitAnd,
    LogicalAnd,
    LogicalOr,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallType {
    Internal,
    External,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOp {
    Keccak256,
}
