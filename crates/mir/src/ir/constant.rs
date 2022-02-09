use std::any::TypeId;

use fe_common::impl_intern_key;
use num_bigint::BigInt;
use smol_str::SmolStr;

use super::{module::ModuleId, SourceInfo};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {
    /// A name of a constant.
    name: SmolStr,

    /// A span where a constant is declared.
    span: SourceInfo,

    /// A value of a constant.
    value: ConstantValue,

    /// A type of a constant.
    ty: TypeId,

    /// A module where a constant is declared.
    module_id: ModuleId,
}

/// An interned Id for [`Constant`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantId(u32);
impl_intern_key!(ConstantId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    String(SmolStr),
    Immediate(BigInt),
}
