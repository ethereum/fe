use crate::names::abi as abi_names;
use crate::operations::data as data_operations;
use crate::utils::ceil_32;
use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, AbiType};
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
pub fn encode<T: AbiEncoding>(types: &[T], vals: Vec<yul::Expression>) -> yul::Expression {
    let func_name = abi_names::encode(&types);
    expression! { [func_name]([vals...]) }
}

/// Returns an expression that gives size of the encoded values.
///
/// It will sum up the sizes known at compile-time with the sizes known during runtime.
pub fn encoding_size<T: AbiEncoding>(types: &[T], vals: Vec<yul::Expression>) -> yul::Expression {
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

/// Returns an expression that gives the size of the encoding's head.
pub fn encoding_head_size<T: AbiEncoding>(types: &[T]) -> yul::Expression {
    literal_expression! { (types.iter().map(|typ| typ.abi_type().head_size()).sum::<usize>()) }
}

/// Returns the known-at-compile-time encoding size.
pub fn encoding_known_size<T: AbiEncoding>(types: &[T]) -> EncodingSize {
    let (min, max) = types.iter().fold((0, 0), |(mut min, mut max), typ| {
        min += typ.abi_type().head_size();
        max += typ.abi_type().head_size();

        match typ.abi_type() {
            AbiType::String { max_size } => {
                min += 32;
                max += ceil_32(max_size) + 32;
            }
            AbiType::Bytes { size } => {
                let size = ceil_32(size) + 32;
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
) -> yul::Statement {
    statement! { abi_unpack([ptr], [array_size], [inner_data_size]) }
}

/// Reverts if the value is not left padded with the given number of bits.
pub fn check_left_padding(size_bits: yul::Expression, val: yul::Expression) -> yul::Statement {
    statement! {
        if (iszero((is_left_padded([size_bits], [val])))) {
            (revert(0, 0))
        }
    }
}

/// Reverts if the value is not right padded with the given number of bits.
pub fn check_right_padding(size_bits: yul::Expression, val: yul::Expression) -> yul::Statement {
    statement! {
        if (iszero((is_right_padded([size_bits], [val])))) {
            (revert(0, 0))
        }
    }
}

/// Reverts if the integer value does not fit within the given number of bytes.
pub fn check_int_size(size: usize, val: yul::Expression) -> yul::Statement {
    // the bits to the left of this size should be either all 0s or all 1s
    let size_bits = literal_expression! { (size * 8 - 1) };
    let is_all_0s = expression! { iszero((shr([size_bits.clone()], [val.clone()]))) };
    let is_all_1s = expression! { iszero((shr([size_bits], (not([val]))))) };
    let is_all_0s_or_1s = expression! { or([is_all_0s], [is_all_1s]) };

    statement! {
        if (iszero([is_all_0s_or_1s])) {
            (revert(0, 0))
        }
    }
}
