use super::{kw, IdentId};
use crate::HirDb;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Bool,
    Int(IntTy),
    Uint(UintTy),
    String,
}

impl PrimTy {
    pub fn name(self, db: &dyn HirDb) -> IdentId {
        match self {
            PrimTy::Bool => kw::BOOL,
            PrimTy::Int(ty) => ty.name(),
            PrimTy::Uint(ty) => ty.name(),
            PrimTy::String => IdentId::new(db, "String".into()),
        }
    }

    pub fn all_types() -> &'static [PrimTy] {
        &[
            PrimTy::Bool,
            PrimTy::Int(IntTy::I8),
            PrimTy::Int(IntTy::I16),
            PrimTy::Int(IntTy::I32),
            PrimTy::Int(IntTy::I64),
            PrimTy::Int(IntTy::I128),
            PrimTy::Int(IntTy::I256),
            PrimTy::Int(IntTy::Isize),
            PrimTy::Uint(UintTy::U8),
            PrimTy::Uint(UintTy::U16),
            PrimTy::Uint(UintTy::U32),
            PrimTy::Uint(UintTy::U64),
            PrimTy::Uint(UintTy::U128),
            PrimTy::Uint(UintTy::U256),
            PrimTy::Uint(UintTy::Usize),
            PrimTy::String,
        ]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    Isize,
}

impl IntTy {
    pub fn name(self) -> IdentId {
        match self {
            IntTy::I8 => kw::I8,
            IntTy::I16 => kw::I16,
            IntTy::I32 => kw::I32,
            IntTy::I64 => kw::I64,
            IntTy::I128 => kw::I128,
            IntTy::I256 => kw::I256,
            IntTy::Isize => kw::Isize,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Usize,
}

impl UintTy {
    pub fn name(self) -> IdentId {
        match self {
            UintTy::U8 => kw::U8,
            UintTy::U16 => kw::U16,
            UintTy::U32 => kw::U32,
            UintTy::U64 => kw::U64,
            UintTy::U128 => kw::U128,
            UintTy::U256 => kw::U256,
            UintTy::Usize => kw::Usize,
        }
    }
}
