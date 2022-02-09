use std::any::TypeId;

use id_arena::Id;

use super::{basic_block::BasicBlockId, function::FunctionId, value::ValueId, SourceInfo};

pub type InstId = Id<Inst>;

pub struct Inst {
    pub source: SourceInfo,
    pub kind: InstKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    /// Unary instruction.
    Unary {
        op: UnOp,
        value: ValueId,
        result: ValueId,
    },

    /// Binary instruction.
    Binary {
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
        result: ValueId,
    },

    /// This is not a real instruction, just used to tag a position where a
    /// local is declared.
    Declare {
        value: ValueId,
    },

    Assign {
        lhs: ValueId,
        rhs: ValueId,
    },

    Cast {
        arg: ValueId,
        to: TypeId,
        result: ValueId,
    },

    /// Constructs aggregate value, i.e. struct, tuple and array.
    AggregateConstruct {
        ty: TypeId,
        args: Vec<ValueId>,
        result: ValueId,
    },

    /// Access to aggregate fields or elements.
    /// # Example
    ///
    /// ```fe
    /// struct Foo:
    ///     x: i32
    ///     y: Array<i32, 8>
    /// ```
    /// `foo.y[2]` is lowered into `AggregateAccess(foo, [1, 2])' for example.
    AggregateAccess {
        value: ValueId,
        indices: Vec<usize>,
        result: ValueId,
    },

    Call {
        func: FunctionId,
        args: Vec<ValueId>,
        result: ValueId,
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

    /// Conditional jump instruction.
    //Branch { args: [Value; 1], dests: [Block; 2] },
    Revert {
        arg: ValueId,
    },

    Emit {
        arg: ValueId,
    },

    Return {
        arg: ValueId,
    },
}

impl InstKind {
    pub fn inst_result(&self) -> Option<ValueId> {
        match self {
            Self::Unary { result, .. }
            | Self::Binary { result, .. }
            | Self::Cast { result, .. }
            | Self::AggregateConstruct { result, .. }
            | Self::AggregateAccess { result, .. }
            | Self::Call { result, .. } => Some(*result),
            Self::Declare { .. }
            | Self::Assign { .. }
            | Self::Jump { .. }
            | Self::Branch { .. }
            | Self::Revert { .. }
            | Self::Emit { .. }
            | Self::Return { .. } => None,
        }
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
