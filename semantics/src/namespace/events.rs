use crate::namespace::types::{
    AbiEncoding,
    FixedSize,
};
use tiny_keccak::{
    Hasher,
    Keccak,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub topic: String,
    fields: Vec<FixedSize>,
    indexed_fields: Vec<usize>,
}

impl Event {
    pub fn new(name: String, fields: Vec<FixedSize>, indexed_fields: Vec<usize>) -> Self {
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
    pub fn fields(&self) -> Vec<(usize, FixedSize)> {
        self.fields
            .to_owned()
            .into_iter()
            .enumerate()
            .filter(|(index, _)| !self.indexed_fields.contains(index))
            .collect()
    }

    /// The event's non-indexed field types.
    pub fn field_types(&self) -> Vec<FixedSize> {
        self.fields().into_iter().map(|(_, typ)| typ).collect()
    }
}

fn build_event_topic(name: String, fields: Vec<String>) -> String {
    sig_keccak256(name, fields, 32)
}

fn sig_keccak256(name: String, params: Vec<String>, size: usize) -> String {
    let signature = format!("{}({})", name, params.join(","));

    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 32];

    keccak.update(signature.as_bytes());
    keccak.finalize(&mut selector);

    format!("0x{}", hex::encode(&selector[0..size]))
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
            "MyEvent".to_string(),
            vec![
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Address),
                FixedSize::Base(Base::Bool),
            ],
            vec![1],
        );

        assert_eq!(
            event.fields(),
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
