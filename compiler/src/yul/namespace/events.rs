use crate::abi::utils as abi_utils;
use crate::errors::CompileError;
use crate::yul::namespace::types::FixedSize;
use yultsur::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub topic: String,
    pub fields: Vec<FixedSize>,
}

impl Event {
    pub fn new(name: String, fields: Vec<FixedSize>) -> Self {
        let abi_fields = fields.iter().map(|f| f.abi_name()).collect::<Vec<String>>();

        let topic = abi_utils::event_topic(name, abi_fields);

        Self { topic, fields }
    }

    pub fn emit(&self, values: Vec<yul::Expression>) -> Result<yul::Statement, CompileError> {
        if let (Some(FixedSize::Array(array)), Some(value)) = (self.fields.first(), values.first())
        {
            let size = literal_expression! {(array.padded_size())};
            let topic = literal_expression! {(self.topic)};

            return Ok(statement! { log1([(*value).clone()], [size], [topic]) });
        }

        Err(CompileError::static_str("can't create emit statement"))
    }
}
