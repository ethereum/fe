use id_arena::Id;
use num_bigint::BigInt;
use smol_str::SmolStr;

use super::{constant::ConstantId, function::FunctionId, inst::InstId, types::TypeId, SourceInfo};

pub type ValueId = Id<Value>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// A value resulted from an instruction.
    Temporary(Temporary),

    /// A local variable declared in a function body.
    Local(Local),

    /// An assignable variable
    AssignableValue(AssignableValue),

    /// An immediate value.
    Immediate(Immediate),

    /// A constant value.
    Constant(Constant),

    /// A singleton value representing `Unit` type.
    Unit(Unit),
}

impl Value {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Temporary(val) => val.ty,
            Self::Local(val) => val.ty,
            Self::AssignableValue(val) => val.ty,
            Self::Immediate(val) => val.ty,
            Self::Constant(val) => val.ty,
            Self::Unit(val) => val.ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Temporary {
    pub inst: InstId,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Immediate {
    pub value: BigInt,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncArg {
    pub func: FunctionId,
    pub idx: usize,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    /// An original name of a local variable.
    pub name: SmolStr,

    pub source: SourceInfo,

    pub ty: TypeId,

    /// `true` if a local is a function argument.
    pub is_arg: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignableValue {
    pub local: ValueId,
    pub indices: Vec<usize>,
    pub source: SourceInfo,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constant {
    pub constant: ConstantId,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unit {
    pub ty: TypeId,
}
