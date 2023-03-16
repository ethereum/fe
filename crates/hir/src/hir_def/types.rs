use super::{Body, GenericArgListId, PathId};

#[salsa::interned]
pub struct TypeId {
    kind: TypeKind,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(TypeId),
    /// The `PathId` is the path to the type, the `Option` is the generic
    /// arguments.
    Path(PathId, Option<GenericArgListId>),
    SelfType,
    /// The `Vec` contains the types of the tuple elements.
    Tuple(Vec<TypeId>),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(TypeId, Body),
    Invalid,
}
