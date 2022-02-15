use id_arena::Id;
use num_bigint::BigInt;
use smol_str::SmolStr;

use super::{constant::ConstantId, inst::InstId, types::TypeId, SourceInfo};

pub type ValueId = Id<Value>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// A value resulted from an instruction.
    Temporary(Temporary),

    /// A local variable declared in a function body.
    Local(Local),

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
            Self::Immediate(val) => val.ty,
            Self::Constant(val) => val.ty,
            Self::Unit(val) => val.ty,
        }
    }
}

macro_rules! embed {
    ($(($variant: expr, $ty: ty)),*) => {
        $(
        impl From<$ty> for Value {
            fn from(val: $ty) -> Self {
                $variant(val)
            }
        })*
    };
}

embed! {
    (Value::Temporary, Temporary),
    (Value::Local, Local),
    (Value::Immediate, Immediate),
    (Value::Constant, Constant),
    (Value::Unit, Unit)
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
pub struct Local {
    /// An original name of a local variable.
    pub name: SmolStr,

    pub ty: TypeId,

    /// `true` if a local is a function argument.
    pub is_arg: bool,

    /// `true` if a local is introduced in MIR.
    pub is_tmp: bool,

    pub source: SourceInfo,
}

impl Local {
    pub fn user_local(name: SmolStr, ty: TypeId, source: SourceInfo) -> Local {
        Self {
            name,
            ty,
            is_arg: false,
            is_tmp: false,
            source,
        }
    }

    pub fn arg_local(name: SmolStr, ty: TypeId, source: SourceInfo) -> Local {
        Self {
            name,
            ty,
            is_arg: true,
            is_tmp: false,
            source,
        }
    }

    pub fn tmp_local(name: SmolStr, ty: TypeId) -> Local {
        Self {
            name,
            ty,
            is_arg: false,
            is_tmp: true,
            source: SourceInfo::dummy(),
        }
    }
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
