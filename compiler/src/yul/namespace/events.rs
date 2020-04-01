use crate::errors::CompileError;
use crate::yul::namespace::types::{Array, Base, FixedSize};
use tiny_keccak::{Hasher, Keccak};
use std::collections::HashMap;
use yultsur::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub topic: [u8; 32],
    pub fields: Vec<FixedSize>,
}

impl Event {
    pub fn new(name: String, fields: Vec<FixedSize>) -> Self {
        let signature = format!(
            "{}({})",
            name,
            fields
                .iter()
                .map(|f| f.abi_name())
                .collect::<Vec<String>>()
                .join(",")
        );

        let mut keccak = Keccak::v256();
        let mut topic = [0u8; 32];

        keccak.update(signature.as_bytes());
        keccak.finalize(&mut topic);

        Self {
            topic,
            fields
        }
    }

    // This only supports byte and u256 arrays at the moment.
    pub fn emit(&self, mut values: Vec<yul::Expression>) -> Result<yul::Statement, CompileError> {
        if let (Some(FixedSize::Array(array)), Some(value)) = (self.fields.first(), values.first())
        {
            let size = literal_expression! {(array.padded_size())};
            let topic = literal_expression! {(format!("0x{}", hex::encode(self.topic)))};

            return Ok(statement! { log1([(*value).clone()], [size], [topic]) });
        }

        Err(CompileError::static_str("Can't create emit statement"))
    }
}
