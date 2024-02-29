use hir::hir_def;
use num_bigint::BigInt;
use smol_str::SmolStr;

#[salsa::interned]
pub struct ConstId {
    #[return_ref]
    pub data: Const,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const {
    pub value: ConstantValue,

    #[return_ref]
    pub(crate) origin: hir_def::Const,
}

// /// An interned Id for [`Constant`].
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct ConstantId(pub(crate) u32);
// impl_intern_key!(ConstantId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Immediate(BigInt),
    Str(SmolStr),
    Bool(bool),
}

// impl From<context::Constant> for ConstantValue {
//     fn from(value: context::Constant) -> Self {
//         match value {
//             context::Constant::Int(num) | context::Constant::Address(num) => Self::Immediate(num),
//             context::Constant::Str(s) => Self::Str(s),
//             context::Constant::Bool(b) => Self::Bool(b),
//         }
//     }
// }
