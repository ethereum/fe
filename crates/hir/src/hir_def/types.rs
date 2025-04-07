use super::{Body, GenericArgListId, Partial, PathId};
use crate::HirDb;

#[salsa::interned]
#[derive(Debug)]
pub struct TypeId<'db> {
    #[return_ref]
    pub data: TypeKind<'db>,
}

impl<'db> TypeId<'db> {
    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        matches!(self.data(db), TypeKind::SelfType(_))
    }

    pub fn fallback_self_ty(db: &'db dyn HirDb) -> Self {
        Self::new(
            db,
            TypeKind::SelfType(GenericArgListId::new(db, Vec::new(), false)),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind<'db> {
    Ptr(Partial<TypeId<'db>>),
    Path(Partial<PathId<'db>>),
    SelfType(GenericArgListId<'db>),
    Tuple(TupleTypeId<'db>),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(Partial<TypeId<'db>>, Partial<Body<'db>>),
    Never,
}

#[salsa::interned]
#[derive(Debug)]
pub struct TupleTypeId<'db> {
    #[return_ref]
    pub data: Vec<Partial<TypeId<'db>>>,
}

impl<'db> TupleTypeId<'db> {
    pub fn to_ty(self, db: &'db dyn HirDb) -> TypeId<'db> {
        TypeId::new(db, TypeKind::Tuple(self))
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}
