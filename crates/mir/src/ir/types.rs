use fe_common::{impl_intern_key, Span};
use smol_str::SmolStr;

use super::module::ModuleId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Bool,
    Address,
    Unit,
    Array(ArrayDef),
    Tuple(TupleDef),
    Struct(StructDef),
    Event(StructDef),
    Contract(StructDef),
    Map(MapDef),
}

/// An interned Id for [`ArrayDef`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);
impl_intern_key!(TypeId);

/// A static array type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayDef {
    pub elem_ty: TypeId,
    pub len: usize,
}

/// A tuple type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleDef {
    pub items: Vec<TypeId>,
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: SmolStr,
    pub fields_ty: Vec<(SmolStr, TypeId)>,
    /// A span where a struct is defined.
    pub span: Span,
    pub module_id: ModuleId,
}

/// A map type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MapDef {
    pub key_ty: TypeId,
    pub value_ty: TypeId,
}
