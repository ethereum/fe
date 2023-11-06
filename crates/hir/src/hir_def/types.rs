use crate::HirDb;

use super::{Body, GenericArgListId, Partial, PathId};

#[salsa::interned]
pub struct TypeId {
    #[return_ref]
    pub data: TypeKind,
}

impl TypeId {
    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        matches!(self.data(db), TypeKind::SelfType(_))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(Partial<TypeId>),
    /// The `PathId` is the path to the type, the `Option` is the generic
    /// arguments.
    Path(Partial<PathId>, GenericArgListId),
    SelfType(GenericArgListId),
    /// The `Vec` contains the types of the tuple elements.
    Tuple(TupleTypeId),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(Partial<TypeId>, Partial<Body>),
}

#[salsa::interned]
pub struct TupleTypeId {
    #[return_ref]
    pub data: Vec<Partial<TypeId>>,
}

impl TupleTypeId {
    pub fn to_ty(self, db: &dyn HirDb) -> TypeId {
        TypeId::new(db, TypeKind::Tuple(self))
    }
}
