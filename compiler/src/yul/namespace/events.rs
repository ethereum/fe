use crate::errors::CompileError;
use crate::yul::namespace::types::{Array, Base, FixedSize};
use std::collections::HashMap;
use yultsur::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub fields: Vec<FixedSize>,
}

impl Event {
    pub fn emit(&self, mut values: Vec<yul::Expression>) -> Result<yul::Statement, CompileError> {
        if let (Some(FixedSize::Array(array)), Some(value)) = (self.fields.first(), values.first())
        {
            let size = literal_expression! {(array.padded_size())};
            return Ok(statement! { log1([(*value).clone()], [size], 0) });
        }

        Err(CompileError::static_str("Can't create emit statement"))
    }
}
