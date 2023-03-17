use super::{Body, GenericArgListId, MaybeInvalid, PathId};

#[salsa::interned]
pub struct TypeId {
    kind: TypeKind,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(MaybeInvalid<TypeId>),
    /// The `PathId` is the path to the type, the `Option` is the generic
    /// arguments.
    Path(MaybeInvalid<PathId>, GenericArgListId),
    SelfType,
    /// The `Vec` contains the types of the tuple elements.
    Tuple(Vec<MaybeInvalid<TypeId>>),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(MaybeInvalid<TypeId>, MaybeInvalid<Body>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitRef {
    pub path: MaybeInvalid<PathId>,
    pub generic_args: GenericArgListId,
}
