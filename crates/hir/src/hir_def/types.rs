use super::{Body, GenericArg, PathId};

#[salsa::interned]
pub struct TypeId {
    kind: TypeKind,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(TypeId),
    Path(PathType),
    SelfType,
    Tuple(TupleType),
    Array(ArrayType),
    Invalid,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PathType {
    pub path: PathId,
    pub args: Vec<GenericArg>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TupleType {
    pub elems: Vec<TypeId>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ArrayType {
    pub elem_ty: TypeId,
    pub len: Body,
}
