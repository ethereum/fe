use super::{Body, GenericArgListId, Partial, PathId};
use crate::HirDb;

#[salsa::interned]
pub struct TypeId {
    #[return_ref]
    pub data: TypeKind,
}

impl TypeId {
    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        matches!(self.data(db), TypeKind::SelfType(_))
    }

    pub fn fallback_self_ty(db: &dyn HirDb) -> Self {
        Self::new(
            db,
            TypeKind::SelfType(GenericArgListId::new(db, Vec::new(), false)),
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Ptr(Partial<TypeId>),
    Path(Partial<PathId>, GenericArgListId),
    SelfType(GenericArgListId),
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

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}
