use crate::names::abi as abi_names;
use crate::operations::data as data_operations;
use crate::utils::ceil_32;
use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, AbiType};
use yultsur::*;

/// Returns an expression that encodes the given values and returns a pointer to
/// the encoding.
pub fn encode<T: AbiEncoding>(types: &[T], vals: Vec<yul::Expression>) -> yul::Expression {
    let func_name = abi_names::encode(&types);
    expression! { [func_name]([vals...]) }
}

/// Returns an expression that gives size of the encoded values.
pub fn encode_size<T: AbiEncoding>(types: &[T], vals: Vec<yul::Expression>) -> yul::Expression {
    let mut head_size = 0;
    let mut known_data_size = 0;
    let mut unknown_data_size = vec![];

    let typed_vals = types.iter().zip(vals);

    for (typ, val) in typed_vals {
        let abi_type = typ.abi_type();
        head_size += abi_type.head_size();
        match abi_type {
            AbiType::String { .. } => {
                known_data_size += 32;
                unknown_data_size.push(expression! { ceil32((mload([val]))) })
            }
            AbiType::Bytes { size } => known_data_size += 32 + ceil_32(size),
            _ => {}
        }
    }

    let static_size = literal_expression! { (head_size + known_data_size) };
    expression! { add([static_size], [data_operations::sum(unknown_data_size)]) }
}

/// Returns an expression that gives the size of the encoding head.
pub fn encode_head_size<T: AbiEncoding>(types: &[T]) -> yul::Expression {
    literal_expression! { (types.iter().map(|typ| typ.abi_type().head_size()).sum::<usize>()) }
}

/// Decode a segment of memory and return each decoded component as separate values.
pub fn decode_data<T: AbiEncoding>(
    types: &[T],
    head_start: yul::Expression,
    data_end: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    let func_name = abi_names::decode_data(types, location);
    expression! { [func_name]([head_start], [data_end]) }
}

/// Decode a single component.
pub fn decode_component(
    typ: AbiType,
    head_start: yul::Expression,
    offset: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    let func_name = abi_names::decode_component(typ, location);
    expression! { [func_name]([head_start], [offset]) }
}

/// Pack each value into a newly allocated segment of memory.
pub fn pack(
    ptr: yul::Expression,
    array_size: yul::Expression,
    inner_data_size: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    match location {
        AbiDecodeLocation::Memory => expression! {
            abi_pack_mem([ptr], [array_size], [inner_data_size])
        },
        AbiDecodeLocation::Calldata => expression! {
            abi_pack_calldata([ptr], [array_size], [inner_data_size])
        },
    }
}

/// Unpack each value into a newly allocated segment of memory.
pub fn unpack(
    ptr: yul::Expression,
    array_size: yul::Expression,
    inner_data_size: yul::Expression,
) -> yul::Statement {
    statement! { abi_unpack([ptr], [array_size], [inner_data_size]) }
}
