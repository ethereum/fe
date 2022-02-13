use fe_analyzer::namespace::types as analyzer_types;

use crate::{db::MirDb, ir::TypeId, lower::types::lower_type};

pub fn lowered_type(db: &dyn MirDb, analyzer_type: analyzer_types::Type) -> TypeId {
    lower_type(db, &analyzer_type)
}
