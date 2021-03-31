use crate::yul::names;
use crate::yul::operations::data as data_operations;
use crate::yul::utils;
use fe_analyzer::namespace::types::{
    AbiArraySize, AbiDecodeLocation, AbiEncoding, AbiType, AbiUintSize,
};
use yultsur::*;

/// Return all abi runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![
        unpack(),
        pack(AbiDecodeLocation::Calldata),
        pack(AbiDecodeLocation::Memory),
    ]
}

/// Creates a batch of encoding function for the given type arrays.
///
/// It sorts the functions and removes duplicates.
pub fn batch_encode<T: AbiEncoding + Ord>(mut batch: Vec<Vec<T>>) -> Vec<yul::Statement> {
    batch.sort();
    batch.dedup();

    batch.into_iter().map(encode).collect()
}

/// Creates a batch of decoding function for the given types and decode
/// locations.
///
/// It sorts the functions and removes duplicates.
pub fn batch_decode<T: AbiEncoding + Ord>(
    mut batch: Vec<(T, AbiDecodeLocation)>,
) -> Vec<yul::Statement> {
    batch.sort();
    batch.dedup();

    batch
        .into_iter()
        .map(|(types, location)| decode(types, location))
        .collect()
}

/// Generates an encoding function for any set of type parameters.
fn encode<T: AbiEncoding>(types: Vec<T>) -> yul::Statement {
    // the name of the function we're generating
    let func_name = names::encode_name(&types);

    // create a vector of identifiers and a vector of tuples, which contain
    // expressions that correspond to the identifiers.
    //
    // The identifier vector is injected into the parameter section of our
    // encoding function and the expressions are used to reference the parameters
    // while encoding.
    let (params, typed_params): (Vec<_>, Vec<_>) = types
        .iter()
        .enumerate()
        .map(|(i, typ)| {
            let ident = identifier! { (format!("val_{}", i)) };
            let expr = identifier_expression! { [ident.clone()] };
            (ident, (expr, typ))
        })
        .unzip();

    // get the size of the static section
    let (_, static_size) = utils::abi_head_offsets(&types);
    let static_size = literal_expression! { (static_size) };

    // we need to keep track of the dynamic section size to properly encode
    // dynamically-sized array headers.
    let mut dyn_sizes = vec![];
    // we also keep a separate vector of dynamically-sized arrays, which we
    // encode after finishing the static section.
    let mut dyn_array_params = vec![];

    // map the typed params to encoding statements for headers and statically-sized
    // values
    let static_encode_stmts = typed_params
        .into_iter()
        .map(|(param, typ)| match typ.abi_type() {
            AbiType::Array {
                inner,
                size: AbiArraySize::Static { size },
            } => encode_static_array(param, *inner, size),
            AbiType::Array {
                inner,
                size: AbiArraySize::Dynamic,
            } => {
                let dyn_offset =
                    expression! { add([static_size.clone()], [data_operations::sum(dyn_sizes.clone())]) };
                let inner = *inner;
                dyn_sizes.push(dyn_array_data_size(param.clone(), inner.clone()));
                dyn_array_params.push((param, inner));
                encode_uint(dyn_offset)
            }
            AbiType::Tuple { elems } => encode_tuple(param, elems),
            AbiType::Uint { .. } => encode_uint(param),
        })
        .collect::<Vec<_>>();

    // map the dynamically-sized arrays to encoding statemnts
    let dyn_encode_stmts = dyn_array_params
        .into_iter()
        .map(|(param, inner)| encode_dyn_array(param, inner))
        .collect::<Vec<_>>();

    function_definition! {
        function [func_name]([params...]) -> ptr {
            (ptr := avail())
            [static_encode_stmts...]
            [dyn_encode_stmts...]
        }
    }
}

/// Generates a decoding function for a single type parameter in either
/// calldata or memory.
fn decode<T: AbiEncoding>(typ: T, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = names::decode_name(&typ, location.clone());

    let decode_expr = match typ.abi_type() {
        AbiType::Uint { .. } => decode_uint(location),
        AbiType::Array { inner, size } => match size {
            AbiArraySize::Static { size } => decode_static_array(*inner, size, location),
            AbiArraySize::Dynamic => decode_dyn_array(*inner, location),
        },
        AbiType::Tuple { elems } => decode_tuple(elems, location),
    };

    function_definition! {
         function [func_name](start_ptr, offset) -> decoded_ptr {
            (let head_ptr := add(start_ptr, offset))
            (decoded_ptr := [decode_expr])
         }
    }
}

/// Adds padding to array elements following the ABI standard.
pub fn unpack() -> yul::Statement {
    function_definition! {
        function abi_unpack(mptr, array_size, inner_data_size) {
            (for {(let i := 0)} (lt(i, array_size)) {(i := add(i, 1))}
            {
                (let val_ptr := add(mptr, (mul(i, inner_data_size))))
                (let val := mloadn(val_ptr, inner_data_size))
                (pop((alloc_mstoren(val, 32))))
            })
        }
    }
}

/// Removes padding from array elements so that they may be stored more
/// efficiently.
pub fn pack(location: AbiDecodeLocation) -> yul::Statement {
    let name = match location {
        AbiDecodeLocation::Calldata => identifier! { abi_pack_calldata },
        AbiDecodeLocation::Memory => identifier! { abi_pack_mem },
    };
    let load = match location {
        AbiDecodeLocation::Calldata => identifier! { calldataload },
        AbiDecodeLocation::Memory => identifier! { mload },
    };

    function_definition! {
        function [name](mptr, array_size, inner_data_size) -> packed_ptr {
            (packed_ptr := avail())

            (for {(let i := 0)} (lt(i, array_size)) {(i := add(i, 1))}
            {
                (let val_ptr := add(mptr, (mul(i, 32))))
                (let val := [load](val_ptr))
                (pop((alloc_mstoren(val, inner_data_size))))
            })
        }
    }
}

fn dyn_array_data_size(val: yul::Expression, inner: AbiType) -> yul::Expression {
    match inner {
        AbiType::Array { .. } => todo!(),
        AbiType::Tuple { .. } => todo!(),
        AbiType::Uint {
            size: AbiUintSize { padded_size, .. },
        } => {
            let inner_padded_size = literal_expression! { (padded_size) };
            let array_size = expression! { mload([val]) };
            let elements_size = expression! { mul([array_size], [inner_padded_size]) };
            expression! { add(32, (ceil32([elements_size]))) }
        }
    }
}

fn encode_tuple(val: yul::Expression, elems: Vec<AbiType>) -> yul::Statement {
    let tuple_size = elems.len() * 32;
    let tuple_size = literal_expression! { (tuple_size) };
    statement! { pop((mcopym([val], [tuple_size]))) }
}

fn encode_uint(val: yul::Expression) -> yul::Statement {
    statement! { pop((alloc_mstoren([val], 32))) }
}

fn encode_dyn_array(val: yul::Expression, inner: AbiType) -> yul::Statement {
    let array_size = expression! { mload([val.clone()]) };
    let array_start = expression! { add([val], 32) };
    block_statement! {
        [encode_uint(array_size.clone())]
        [encode_array(array_start, inner, array_size)]
    }
}

fn encode_static_array(val: yul::Expression, inner: AbiType, size: usize) -> yul::Statement {
    let array_size = literal_expression! { (size) };
    encode_array(val, inner, array_size)
}

fn encode_array(
    val: yul::Expression,
    inner: AbiType,
    array_size: yul::Expression,
) -> yul::Statement {
    match inner {
        AbiType::Array { .. } => todo!("encoding of nested arrays"),
        AbiType::Tuple { .. } => todo!("encoding of nested tuples"),
        AbiType::Uint {
            size:
                AbiUintSize {
                    data_size,
                    padded_size,
                },
        } => {
            let inner_data_size = literal_expression! { (data_size) };

            if data_size == padded_size {
                // there's no need to unpack if the inner element has no padding
                let array_data_size = expression! {
                    mul([array_size], [inner_data_size])
                };
                let right_padding = expression! {
                    sub((ceil32(array_data_size)), array_data_size)
                };

                block_statement! {
                    (let array_data_size := [array_data_size])
                    (pop((mcopym([val], array_data_size))))
                    (pop((alloc([right_padding]))))
                }
            } else {
                statement! { abi_unpack([val], [array_size], [inner_data_size]) }
            }
        }
    }
}

fn decode_uint(location: AbiDecodeLocation) -> yul::Expression {
    match location {
        AbiDecodeLocation::Memory => expression! { mload(head_ptr) },
        AbiDecodeLocation::Calldata => expression! { calldataload(head_ptr) },
    }
}

fn decode_dyn_array(inner: AbiType, location: AbiDecodeLocation) -> yul::Expression {
    let load = match location {
        AbiDecodeLocation::Calldata => {
            identifier! { calldataload }
        }
        AbiDecodeLocation::Memory => {
            identifier! { mload }
        }
    };

    let encoding_start = expression! { add(start_ptr, ([load.clone()](head_ptr))) };
    let array_size = expression! { [load]([encoding_start.clone()]) };

    match inner {
        AbiType::Array { .. } => todo!("decoding of nested arrays"),
        AbiType::Tuple { .. } => todo!("decoding of nested tuples"),
        AbiType::Uint {
            size:
                AbiUintSize {
                    padded_size,
                    data_size,
                },
        } => {
            let inner_data_size = literal_expression! { (data_size) };

            if padded_size == data_size {
                match location {
                    AbiDecodeLocation::Calldata => {
                        let array_data_size = expression! { mul([array_size], [inner_data_size]) };
                        expression! { ccopym([encoding_start], (add([array_data_size], 32))) }
                    }
                    AbiDecodeLocation::Memory => {
                        expression! { [encoding_start] }
                    }
                }
            } else {
                unimplemented!("packing of dynamically sized arrays")
            }
        }
    }
}

fn decode_static_array(
    inner: AbiType,
    size: usize,
    location: AbiDecodeLocation,
) -> yul::Expression {
    let array_size = literal_expression! { (size) };
    let array_start = identifier_expression! { head_ptr };

    match inner {
        AbiType::Array { .. } => todo!("decoding of nested arrays"),
        AbiType::Tuple { .. } => todo!("decoding of nested tuples"),
        AbiType::Uint {
            size:
                AbiUintSize {
                    padded_size,
                    data_size,
                },
        } => {
            let inner_data_size = literal_expression! { (data_size) };

            if padded_size == data_size {
                match location {
                    // need to include the length
                    AbiDecodeLocation::Calldata => {
                        let array_data_size = expression! { mul([array_size], [inner_data_size]) };
                        expression! { ccopym([array_start], [array_data_size]) }
                    }
                    AbiDecodeLocation::Memory => array_start,
                }
            } else {
                match location {
                    AbiDecodeLocation::Calldata => {
                        expression! {
                            abi_pack_calldata([array_start], [array_size], [inner_data_size])
                        }
                    }
                    AbiDecodeLocation::Memory => {
                        expression! {
                            abi_pack_mem([array_start], [array_size], [inner_data_size])
                        }
                    }
                }
            }
        }
    }
}

fn decode_tuple(elems: Vec<AbiType>, location: AbiDecodeLocation) -> yul::Expression {
    let tuple_size = elems.len() * 32;
    let tuple_size = literal_expression! { (tuple_size) };

    match location {
        AbiDecodeLocation::Memory => expression! { head_ptr },
        AbiDecodeLocation::Calldata => expression! { ccopym(head_ptr, [tuple_size]) },
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::runtime::functions::abi::{decode, encode};
    use fe_analyzer::namespace::types::{AbiDecodeLocation, Base, FeString, U256};

    #[test]
    fn test_encode() {
        assert_eq!(
            encode(vec![U256, Base::Address]).to_string(),
            "function abi_encode_u256_address(val_0, val_1) -> ptr { ptr := avail() pop(alloc_mstoren(val_0, 32)) pop(alloc_mstoren(val_1, 32)) }"
        )
    }

    #[test]
    fn test_decode_string_mem() {
        assert_eq!(
            decode(FeString { max_size: 100 }, AbiDecodeLocation::Memory).to_string(),
            "function abi_decode_string_100_mem(start_ptr, offset) -> decoded_ptr { let head_ptr := add(start_ptr, offset) decoded_ptr := add(start_ptr, mload(head_ptr)) }"
        )
    }

    #[test]
    fn test_decode_string_calldata() {
        assert_eq!(
            decode(FeString { max_size: 100 }, AbiDecodeLocation::Calldata).to_string(),
            "function abi_decode_string_100_calldata(start_ptr, offset) -> decoded_ptr { let head_ptr := add(start_ptr, offset) decoded_ptr := ccopym(add(start_ptr, calldataload(head_ptr)), add(mul(calldataload(add(start_ptr, calldataload(head_ptr))), 1), 32)) }"
        )
    }

    #[test]
    fn test_decode_u256_mem() {
        assert_eq!(
            decode(U256, AbiDecodeLocation::Memory).to_string(),
            "function abi_decode_u256_mem(start_ptr, offset) -> decoded_ptr { let head_ptr := add(start_ptr, offset) decoded_ptr := mload(head_ptr) }"
        )
    }
}
