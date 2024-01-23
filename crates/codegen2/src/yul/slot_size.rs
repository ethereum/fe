use fe_mir::ir::{Type, TypeId, TypeKind};

use crate::db::CodegenDb;

// We use the same slot size between memory and storage to simplify the
// implementation and minimize gas consumption in memory <-> storage copy
// instructions.
pub(crate) const SLOT_SIZE: usize = 32;

pub(crate) fn yul_primitive_type(db: &dyn CodegenDb) -> TypeId {
    db.mir_intern_type(Type::new(TypeKind::U256, None).into())
}

pub(crate) fn function_hash_type(db: &dyn CodegenDb) -> TypeId {
    db.mir_intern_type(Type::new(TypeKind::U32, None).into())
}
