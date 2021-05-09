use crate::errors::SemanticError;
use crate::namespace::types::{FixedSize, Type};
use std::convert::TryInto;

pub fn types_to_fixed_sizes(sizes: &[Type]) -> Result<Vec<FixedSize>, SemanticError> {
    sizes.iter().map(|param| param.clone().try_into()).collect()
}
