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
    Unit,
    Array(ArrayDef),
    Tuple(TupleDef),
    Struct(StructDef),
    Contract(ContractDef),
    Map(MapDef),
}

/// An interned Id for [`ArrayDef`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);
impl_intern_key!(TypeId);

/// A static array type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayDef {
    ty: TypeId,
    len: usize,
}

/// A tuple type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleDef {
    items: Vec<Type>,
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    name: SmolStr,
    fields: Vec<(SmolStr, TypeId)>,
    /// A span where a struct is defined.
    span: Span,
    module_id: ModuleId,
}

/// A user defined contract type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractDef {
    name: SmolStr,
    fields: Vec<(SmolStr, TypeId)>,
    /// A span where a struct is defined.
    span: Span,
    module_id: ModuleId,
}

/// A map type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MapDef {
    key: TypeId,
    value: TypeId,
}
