use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};

use crate::{
    db::MirDb,
    ir::TypeId,
    lower::types::{lower_event_type, lower_type},
};

pub fn mir_lowered_type(db: &dyn MirDb, analyzer_type: analyzer_types::Type) -> TypeId {
    lower_type(db, &analyzer_type)
}

pub fn mir_lowered_event_type(db: &dyn MirDb, event: analyzer_items::EventId) -> TypeId {
    lower_event_type(db, event)
}
