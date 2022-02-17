use crate::names::abi as abi_names;
use crate::operations::data as data_operations;
use crate::types::{AbiDecodeLocation, AbiType};
use crate::utils::ceil_32;
use yultsur::*;

/// The size of an encoding known at compile-time.
///
/// The expressions should only be given literal values.
pub enum EncodingSize {
    Exact(yul::Expression),
    Bounded {
        min: yul::Expression,
        max: yul::Expression,
    },
}

/// Returns an expression that encodes the given values and returns a pointer to
/// the encoding.
pub fn encode(types: &[AbiType], vals: Vec<yul::Expression>) -> yul::Expression {
    let func_name = abi_names::encode(types);
    expression! { [func_name]([vals...]) }
}

/// Returns an expression that gives size of the encoded values.
///
/// It will sum up the sizes known at compile-time with the sizes known during runtime.
pub fn encoding_size(types: &[AbiType], vals: &[yul::Expression]) -> yul::Expression {
    let mut head_size = 0;
    let mut known_data_size = 0;
    let mut unknown_data_size = vec![];

    let typed_vals = types.iter().zip(vals);

    for (typ, val) in typed_vals {
        head_size += typ.head_size();
        match typ {
            AbiType::String { .. } => {
                known_data_size += 32;
                unknown_data_size.push(expression! { ceil32((mload([val.clone()]))) })
            }
            AbiType::Bytes { size } => known_data_size += 32 + ceil_32(*size),
            _ => {}
        }
    }

    let static_size = literal_expression! { (head_size + known_data_size) };
    expression! { add([static_size], [data_operations::sum(unknown_data_size)]) }
}

/// Returns an expression that gives the size of the encoding's head.
pub fn encoding_head_size(types: &[AbiType]) -> yul::Expression {
    literal_expression! { (types.iter().map(AbiType::head_size).sum::<usize>()) }
}

/// Returns the known-at-compile-time encoding size.
pub fn encoding_known_size(types: &[AbiType]) -> EncodingSize {
    let (min, max) = types.iter().fold((0, 0), |(mut min, mut max), typ| {
        min += typ.head_size();
        max += typ.head_size();

        match typ {
            AbiType::String { max_size } => {
                min += 32;
                max += ceil_32(*max_size) + 32;
            }
            AbiType::Bytes { size } => {
                let size = ceil_32(*size) + 32;
                min += size;
                max += size;
            }
            _ => {}
        }

        (min, max)
    });

    if min == max {
        EncodingSize::Exact(literal_expression! { (min) })
    } else {
        EncodingSize::Bounded {
            min: literal_expression! { (min) },
            max: literal_expression! { (max) },
        }
    }
}

/// Decode a segment of memory and return each decoded component as separate values.
pub fn decode_data(
    types: &[AbiType],
    head_start: yul::Expression,
    data_end: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    let func_name = abi_names::decode_data(types, location);
    expression! { [func_name]([head_start], [data_end]) }
}

/// Decode a single component.
pub fn decode_component(
    typ: &AbiType,
    head_start: yul::Expression,
    offset: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    let func_name = abi_names::decode_component(typ, location);
    expression! { [func_name]([head_start], [offset]) }
}

/// Unpack each value into a newly allocated segment of memory.
pub fn unpack(
    ptr: yul::Expression,
    array_size: yul::Expression,
    inner_data_size: yul::Expression,
    signed: yul::Expression,
) -> yul::Statement {
    statement! { abi_unpack([ptr], [array_size], [inner_data_size], [signed]) }
}
