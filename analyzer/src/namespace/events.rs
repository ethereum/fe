use crate::namespace::types::{
    AbiEncoding,
    FixedSize,
};
use fe_common::utils::keccak::get_full_signature;

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub topic: String,
    fields: Vec<FixedSize>,
    indexed_fields: Vec<usize>,
}

impl Event {
    pub fn new(name: &str, fields: Vec<FixedSize>, indexed_fields: Vec<usize>) -> Self {
        let abi_fields = fields
            .iter()
            .map(|field| field.abi_name())
            .collect::<Vec<String>>();
        let topic = build_event_topic(name, abi_fields);

        Self {
            topic,
            fields,
            indexed_fields,
        }
    }

    /// The event's indexed fields.
    ///
    /// These should be logged as additional topics.
    pub fn indexed_fields(&self) -> Vec<(usize, FixedSize)> {
        self.indexed_fields
            .to_owned()
            .into_iter()
            .map(|index| (index, self.fields[index].to_owned()))
            .collect()
    }

    /// The event's non-indexed fields.
    ///
    /// These should be logged in the data section.
    pub fn non_indexed_fields(&self) -> Vec<(usize, FixedSize)> {
        self.fields
            .to_owned()
            .into_iter()
            .enumerate()
            .filter(|(index, _)| !self.indexed_fields.contains(index))
            .collect()
    }

    /// The event's non-indexed field types.
    pub fn non_indexed_field_types(&self) -> Vec<FixedSize> {
        self.non_indexed_fields()
            .into_iter()
            .map(|(_, typ)| typ)
            .collect()
    }
    /// The event's field types.
    pub fn field_types(&self) -> Vec<FixedSize> {
        self.fields.clone()
    }
}

fn build_event_topic(name: &str, fields: Vec<String>) -> String {
    let signature = format!("{}({})", name, fields.join(","));
    get_full_signature(signature.as_bytes())
}

#[cfg(test)]
mod tests {
    use crate::namespace::events::Event;
    use crate::namespace::types::{
        Base,
        FixedSize,
    };

    #[test]
    fn test_new_event() {
        let event = Event::new(
            "MyEvent",
            vec![
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Bool),
            ],
            vec![1],
        );

        assert_eq!(
            event.field_types(),
            vec![
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Bool),
            ],
        );

        assert_eq!(
            event.non_indexed_fields(),
            vec![
                (0, FixedSize::Base(Base::Address)),
                (2, FixedSize::Base(Base::Bool))
            ]
        );

        assert_eq!(
            event.indexed_fields(),
            vec![(1, FixedSize::Base(Base::Address))]
        );
    }
}
