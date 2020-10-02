use crate::namespace::types::FixedSize;
use tiny_keccak::{
    Hasher,
    Keccak,
};
use yultsur::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub topic: String,
    pub fields: Vec<FixedSize>,
}

impl Event {
    pub fn new(name: String, fields: Vec<FixedSize>) -> Self {
        let abi_fields = fields
            .iter()
            .map(|field| field.abi_name())
            .collect::<Vec<String>>();
        let topic = event_topic(name, abi_fields);

        Self { topic, fields }
    }

    pub fn emit(&self, values: Vec<yul::Expression>) -> yul::Statement {
        if let (Some(FixedSize::Array(array)), Some(value)) = (self.fields.first(), values.first())
        {
            let size = literal_expression! {(array.padded_size())};
            let topic = literal_expression! {(self.topic)};

            return statement! { log1([(*value).clone()], [size], [topic]) };
        }

        unimplemented!()
    }
}

/// Formats the name and fields and calculates the 32 byte keccak256 value of
/// the signature.
pub fn event_topic(name: String, fields: Vec<String>) -> String {
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
