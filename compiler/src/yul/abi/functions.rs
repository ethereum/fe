use crate::yul::abi::utils::{
    decode_name,
    encode_name,
    head_offsets,
};
use crate::yul::operations;
use fe_semantics::namespace::types::{
    AbiArraySize,
    AbiDecodeLocation,
    AbiEncoding,
    AbiType,
    AbiUintSize,
};
use yultsur::*;

/// Generates an encoding function for any set of type parameters.
pub fn encode<T: AbiEncoding>(types: Vec<T>) -> yul::Statement {
    // the name of the function we're generating
    let func_name = encode_name(&types);

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
    let (_, static_size) = head_offsets(&types);
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
                    expression! { add([static_size.clone()], [operations::sum(dyn_sizes.clone())]) };
                let inner = *inner;
                dyn_sizes.push(dyn_array_data_size(param.clone(), inner.clone()));
                dyn_array_params.push((param, inner));
                encode_uint(dyn_offset)
            }
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

fn dyn_array_data_size(val: yul::Expression, inner: AbiType) -> yul::Expression {
    match inner {
        AbiType::Array { .. } => unimplemented!(),
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
    let (inner_data_size, inner_padded_size) = match inner {
        AbiType::Uint {
            size:
                AbiUintSize {
                    data_size,
                    padded_size,
                },
        } => (
            literal_expression! { (data_size) },
            literal_expression! { (padded_size) },
        ),
        AbiType::Array { .. } => unimplemented!(),
    };

    block_statement! {
        (let array_size := [array_size])
        (let array_data_size := mul(array_size, [inner_padded_size.clone()]))
        (for {(let i := 0)} (lt(i, array_size)) {(i := add(i, 1))}
        {
            (let val_ptr := add([val], (mul(i, [inner_data_size.clone()]))))
            (let val := mloadn(val_ptr, [inner_data_size]))
            (pop((alloc_mstoren(val, [inner_padded_size]))))
        })
        (pop((alloc((sub((ceil32(array_data_size)), array_data_size))))))
    }
}

/// Generates an decoding function for a single type parameter in either
/// calldata or memory.
pub fn decode<T: AbiEncoding>(typ: T, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = decode_name(&typ, location.clone());

    let decode_expr = match typ.abi_type() {
        AbiType::Uint { .. } => decode_uint(location),
        AbiType::Array { inner, size } => decode_array(*inner, size, location),
    };

    function_definition! {
         // `start` refers to the beginning of the encoding
         // `head_ptr` refers to the pointer at which the head is located
         function [func_name](start, head_ptr) -> val {
            (val := [decode_expr])
         }
    }
}

fn decode_uint(location: AbiDecodeLocation) -> yul::Expression {
    match location {
        AbiDecodeLocation::Memory => expression! { mload(head_ptr) },
        AbiDecodeLocation::Calldata => expression! { calldataload(head_ptr) },
    }
}

fn decode_array(
    inner: AbiType,
    size: AbiArraySize,
    location: AbiDecodeLocation,
) -> yul::Expression {
    match inner {
        // encoding of nested arrays
        AbiType::Array { .. } => unimplemented!(),
        // encoding of values that do not need to be unpacked
        AbiType::Uint {
            size:
                AbiUintSize {
                    padded_size,
                    data_size,
                },
        } if padded_size == data_size => match size {
            AbiArraySize::Static { size } => {
                let total_size = size * data_size;
                let total_size = literal_expression! { (total_size) };

                match location {
                    AbiDecodeLocation::Calldata => {
                        expression! { ccopym(head_ptr, [total_size]) }
                    }
                    AbiDecodeLocation::Memory => expression! { head_ptr },
                }
            }
            AbiArraySize::Dynamic => match location {
                AbiDecodeLocation::Calldata => {
                    let data_ptr = expression! { add(start, (calldataload(head_ptr))) };
                    let array_size = expression! { calldataload([data_ptr.clone()]) };
                    let data_size = literal_expression! { (data_size) };
                    let total_size = expression! { add(32, (mul([array_size], [data_size]))) };

                    expression! { ccopym([data_ptr], [total_size]) }
                }
                AbiDecodeLocation::Memory => {
                    expression! { add(start, (mload(head_ptr))) }
                }
            },
        },
        // encoding of values that need to be packed
        AbiType::Uint { .. } => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::abi::functions::{
        decode,
        encode,
    };
    use fe_semantics::namespace::types::{
        AbiDecodeLocation,
        Base,
        FeString,
        U256,
    };

    #[test]
    fn test_encode() {
        assert_eq!(
            encode(vec![U256, Base::Address]).to_string(),
            "function abi_encode_uint256_address(val_0, val_1) -> ptr { ptr := avail() pop(alloc_mstoren(val_0, 32)) pop(alloc_mstoren(val_1, 32)) }"
        )
    }

    #[test]
    fn test_decode_string_mem() {
        assert_eq!(
            decode(FeString { max_size: 100 }, AbiDecodeLocation::Memory).to_string(),
            "function abi_decode_string100_mem(start, head_ptr) -> val { val := add(start, mload(head_ptr)) }"
        )
    }

    #[test]
    fn test_decode_string_calldata() {
        assert_eq!(
            decode(FeString { max_size: 100 }, AbiDecodeLocation::Calldata).to_string(),
            "function abi_decode_string100_calldata(start, head_ptr) -> val { val := ccopym(add(start, calldataload(head_ptr)), add(32, mul(calldataload(add(start, calldataload(head_ptr))), 1))) }"
        )
    }

    #[test]
    fn test_decode_u256_mem() {
        assert_eq!(
            decode(U256, AbiDecodeLocation::Memory).to_string(),
            "function abi_decode_uint256_mem(start, head_ptr) -> val { val := mload(head_ptr) }"
        )
    }
}
