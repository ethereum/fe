use crate::namespace::types::{AbiEncoding, FixedSize};
use fe_common::utils::keccak;
use indexmap::IndexMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EventDef {
    pub name: String,
    pub fields: Vec<EventField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EventField {
    pub name: String,
    pub typ: FixedSize,
    pub is_indexed: bool,
}

impl EventDef {
    // /// The event's indexed fields.
    // ///
    // /// These should be logged as additional topics.
    // pub fn indexed_field_types_with_index(&self) -> Vec<(usize, FixedSize)> {
    //     self.indexed_fields
    //         .to_owned()
    //         .into_iter()
    //         .map(|index| (index, self.fields[index].1.to_owned()))
    //         .collect()
    // }

    // /// The event's non-indexed fields.
    // ///
    // /// These should be logged in the data section.
    // pub fn non_indexed_field_types_with_index(&self) -> Vec<(usize, FixedSize)> {
    //     self.fields
    //         .to_owned()
    //         .into_iter()
    //         .enumerate()
    //         .filter(|(index, _)| !self.indexed_fields.contains(index))
    //         .map(|(index, (_, typ))| (index, typ))
    //         .collect()
    // }

    // /// The event's non-indexed field types.
    // pub fn non_indexed_field_types(&self) -> Vec<FixedSize> {
    //     self.non_indexed_field_types_with_index()
    //         .into_iter()
    //         .map(|(_, typ)| typ)
    //         .collect()
    // }
}

// fn build_event_topic(name: &str, fields: Vec<String>) -> String {
//     let signature = format!("{}({})", name, fields.join(","));
//     keccak::full(signature.as_bytes())
// }
