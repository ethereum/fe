use super::{Body, GenericArgListId, Partial, PathId};

#[salsa::interned]
pub struct TypeId {
    pub data: TypeKind,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(Partial<TypeId>),
    /// The `PathId` is the path to the type, the `Option` is the generic
    /// arguments.
    Path(Partial<PathId>, GenericArgListId),
    SelfType,
    /// The `Vec` contains the types of the tuple elements.
    Tuple(Vec<Partial<TypeId>>),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(Partial<TypeId>, Partial<Body>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitRef {
    pub path: Partial<PathId>,
    pub generic_args: GenericArgListId,
}
