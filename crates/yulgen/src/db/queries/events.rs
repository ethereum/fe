use crate::db::YulgenDb;
use crate::types::{AbiType, AsAbiType};
use fe_analyzer::namespace::items::EventId;
use std::rc::Rc;

pub fn event_idx_abi_types(db: &dyn YulgenDb, event: EventId) -> Rc<[AbiType]> {
    event
        .typ(db.upcast())
        .fields
        .iter()
        .filter_map(|field| {
            (!field.is_indexed).then(|| {
                field
                    .typ
                    .clone()
                    .expect("event field type error")
                    .as_abi_type(db.upcast())
            })
        })
        .collect::<Vec<_>>()
        .into()
}
