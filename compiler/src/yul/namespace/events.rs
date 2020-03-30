use crate::errors::CompileError;
use yultsur::*;
use crate::yul::namespace::types::{FixedSize, Array, Base};
use std::collections::HashMap;


#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub fields: Vec<FixedSize>
}

impl Event {
    pub fn emit(
        &self,
        mut values: Vec<yul::Expression>
    ) -> Result<yul::Statement, CompileError> {
        if let (Some(FixedSize::Array(array)), Some(value)) = (self.fields.first(), values.first()) {
            let size = literal_expression! {(array.size())};
            return Ok(statement! { log1([(*value).clone()], [size], 0) });
        }

        Err(CompileError::static_str("Can't create emit statement"))
    }
}