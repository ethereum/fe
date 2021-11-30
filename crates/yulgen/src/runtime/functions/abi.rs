use crate::constants::ERROR_INVALID_ABI_DATA;
use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::operations::abi::EncodingSize;
use crate::operations::revert as revert_operations;
use crate::types::{AbiDecodeLocation, AbiType};
use crate::utils::ceil_32;
use yultsur::*;

/// Return all abi runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![
        unpack(),
        is_left_padded(),
        is_right_padded(),
        // This is needed for `revert_with_Panic_uint256`, which is included in the std batch of
        // revert functions.
        // It will be removed in https://github.com/ethereum/fe/pull/478 along with all other
        // batches of runtime functions.
        encode(&[AbiType::Uint { size: 32 }]),
    ]
}

/// Returns a yul function that decodes a block of abi-encoded data into the
/// specified [`AbiType`] componenets, eg `abi_decode_data_u256_Foo_u8_calldata`.
/// The decoding of each component is handled by a separate function, eg.
/// `abi_decode_component_uint32_mem`; these component decoding functions
/// are also included in the returned `Vec`.
pub fn decode_functions(types: &[AbiType], location: AbiDecodeLocation) -> Vec<yul::Statement> {
    let mut component_fns: Vec<_> = types.iter().fold(vec![], |mut funcs, typ| {
        funcs.push(decode_component(typ, location));
        match typ {
            AbiType::Tuple { components } => {
                for ctyp in components {
                    funcs.push(decode_component(ctyp, location))
                }
            }
            AbiType::StaticArray { inner, .. } => funcs.push(decode_component(inner, location)),
            _ => {}
        };
        funcs
    });

    component_fns.sort();
    component_fns.dedup();
    component_fns.push(decode_data(types, location));
    component_fns
}

/// Creates a function that decodes ABI encoded data.
fn decode_data(types: &[AbiType], location: AbiDecodeLocation) -> yul::Statement {
    #[derive(Clone)]
    struct IdentExpr {
        ident: yul::Identifier,
        expr: yul::Expression,
    }

    impl From<String> for IdentExpr {
        fn from(string: String) -> Self {
            IdentExpr {
                ident: identifier! { (string) },
                expr: identifier_expression! { (string) },
            }
        }
    }

    #[derive(Clone)]
    struct DataOffsets {
        start: IdentExpr,
        end: IdentExpr,
    }

    #[derive(Clone)]
    struct DecodeVal {
        typ: AbiType,
        return_val: IdentExpr,
        decoded_val: IdentExpr,
        head_offset: IdentExpr,
        data_offsets: Option<DataOffsets>,
    }

    let func_name = abi_names::decode_data(types, location);

    let vals: Vec<DecodeVal> = types
        .iter()
        .enumerate()
        .map(|(i, typ)| {
            let data_offsets = if typ.has_data() {
                Some(DataOffsets {
                    start: format!("data_start_offset_{}", i).into(),
                    end: format!("data_end_offset_{}", i).into(),
                })
            } else {
                None
            };

            DecodeVal {
                typ: typ.to_owned(),
                return_val: format!("return_val_{}", i).into(),
                decoded_val: format!("decoded_val_{}", i).into(),
                head_offset: format!("head_offset_{}", i).into(),
                data_offsets,
            }
        })
        .collect();

    let size_check = match abi_operations::encoding_known_size(types) {
        EncodingSize::Exact(size) => statements! {
            (let encoding_size := sub(data_end, head_start))
            (if (iszero((eq(encoding_size, [size])))) {
                [revert_with_invalid_abi_data()]
            })
        },
        EncodingSize::Bounded { min, max } => statements! {
            (let encoding_size := sub(data_end, head_start))
            (if (or(
                (lt(encoding_size, [min])),
                (gt(encoding_size, [max]))
            )) {
                [revert_with_invalid_abi_data()]
            })
        },
    };

    let return_val_idents: Vec<_> = vals
        .clone()
        .into_iter()
        .map(|val| val.return_val.ident)
        .collect();

    // Create declaration statements for each offset.
    let head_offset_decls: Vec<_> = {
        let mut curr_offset = 0;
        vals.clone()
            .into_iter()
            .map(|val| {
                let head_offset = literal_expression! { (curr_offset) };
                let head_offset_decl = statement! { let [val.head_offset.ident] := [head_offset] };
                curr_offset += val.typ.head_size();
                head_offset_decl
            })
            .collect()
    };

    // Call the component decoding functions for each offset value.
    let decode_stmts: Vec<_> = vals
        .clone()
        .into_iter()
        .map(|val| {
            let target_idents = if let Some(data_offsets) = val.data_offsets {
                identifiers! {
                    [val.decoded_val.ident]
                    [data_offsets.start.ident]
                    [data_offsets.end.ident]
                }
            } else {
                identifiers! { [val.decoded_val.ident] }
            };

            let decode_expr = abi_operations::decode_component(
                &val.typ,
                expression! { head_start },
                val.head_offset.expr,
                location,
            );
            statement! { let [target_idents...] := [decode_expr] }
        })
        .collect();

    let encoding_head_size = abi_operations::encoding_head_size(types);
    let data_offset_checks: Vec<_> = {
        let (mut start_offset_exprs, mut end_offset_exprs): (Vec<_>, Vec<_>) = vals
            .clone()
            .into_iter()
            .filter_map(|val| {
                val.data_offsets
                    .map(|data_offsets| (data_offsets.start.expr, data_offsets.end.expr))
            })
            .unzip();

        start_offset_exprs.push(expression! { encoding_size });
        end_offset_exprs.insert(0, encoding_head_size);

        start_offset_exprs
            .into_iter()
            .zip(end_offset_exprs)
            .map(|(start_offset, end_offset)| {
                statement! {
                    if (iszero((eq([start_offset], [end_offset])))) { [revert_with_invalid_abi_data()] }
                }
            })
            .collect()
    };

    let return_assignments: Vec<_> = vals
        .into_iter()
        .map(|val| statement! { [val.return_val.ident] := [val.decoded_val.expr] })
        .collect();

    function_definition! {
         function [func_name](head_start, data_end) -> [return_val_idents...] {
            [size_check...]
            [head_offset_decls...]
            [decode_stmts...]
            [data_offset_checks...]
            [return_assignments...]
         }
    }
}

/// Creates a function that decodes a single component in ABI encoded data.
pub fn decode_component(typ: &AbiType, location: AbiDecodeLocation) -> yul::Statement {
    match typ {
        AbiType::StaticArray { inner, size } => {
            decode_component_static_array(inner, *size, location)
        }
        AbiType::Tuple { components: elems } => decode_component_tuple(elems, location),
        AbiType::Uint { size } => decode_component_uint(*size, location),
        AbiType::Int { size } => decode_component_int(*size, location),
        AbiType::Bool => decode_component_bool(location),
        AbiType::Address => decode_component_address(location),
        AbiType::String { max_size } => decode_component_string(*max_size, location),
        AbiType::Bytes { size } => decode_component_bytes(*size, location),
    }
}

pub fn decode_component_uint(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_uint(size, location);
    let decode_expr = load_word(expression! { ptr }, location);
    let check_padding = check_left_padding(
        literal_expression! { ((32 - size) * 8) },
        expression! { return_val },
    );

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
            [check_padding]
         }
    }
}

pub fn decode_component_int(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_int(size, location);
    let decode_expr = load_word(expression! { ptr }, location);
    let check_size = check_int_size(size, expression! { return_val });

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
            [check_size]
         }
    }
}

pub fn decode_component_bool(location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_bool(location);
    let decode_expr = load_word(expression! { ptr }, location);
    let check_padding = check_left_padding(expression! { 255 }, expression! { return_val });

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
            [check_padding]
         }
    }
}

pub fn decode_component_address(location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_address(location);
    let decode_expr = load_word(expression! { ptr }, location);
    let check_padding = check_left_padding(expression! { 96 }, expression! { return_val });

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
            [check_padding]
         }
    }
}

pub fn decode_component_static_array(
    inner: &AbiType,
    array_size: usize,
    location: AbiDecodeLocation,
) -> yul::Statement {
    let func_name = abi_names::decode_component_static_array(inner, array_size, location);
    let array_size = literal_expression! { (array_size) };
    let inner_packed_size = literal_expression! { (inner.packed_size()) };
    let decode_inner_expr = abi_operations::decode_component(
        inner,
        expression! { head_start },
        expression! { inner_offset },
        location,
    );

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := avail())
            (for {(let i := 0)} (lt(i, [array_size])) {(i := add(i, 1))}
            {
                (let inner_offset := add(offset, (mul(i, 32))))
                (let decoded_val := [decode_inner_expr])
                (pop((alloc_mstoren(decoded_val, [inner_packed_size]))))
            })
         }
    }
}

pub fn decode_component_tuple(elems: &[AbiType], location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_tuple(elems, location);
    let decode_stmts: Vec<_> = elems
        .iter()
        .enumerate()
        .map(|(index, component)| {
            let decode_component_expr = abi_operations::decode_component(
                component,
                expression! { head_start },
                expression! { component_offset },
                location,
            );
            let index = literal_expression! { (index) };

            block_statement! {
                (let component_offset := add(offset, (mul(32, [index]))))
                (let decoded_val := [decode_component_expr])
                (pop((alloc_mstoren(decoded_val, 32))))
            }
        })
        .collect();

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := avail())
            [decode_stmts...]
         }
    }
}

pub fn decode_component_bytes(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_bytes(size, location);
    let size = literal_expression! { (size) };

    function_definition! {
         function [func_name](head_start, head_offset) -> return_val, data_start_offset, data_end_offset {
            (let head_ptr := add(head_start, head_offset))
            (data_start_offset := [load_word(expression! { head_ptr }, location)])
            (let data_start := add(head_start, data_start_offset))
            (let bytes_size := [load_word(expression! { data_start }, location)])
            (if (iszero((eq(bytes_size, [size])))) { [revert_with_invalid_abi_data()] } )
            (let data_size := add(bytes_size, 32))
            (let padded_data_size := ceil32(data_size))
            (data_end_offset := add(data_start_offset, padded_data_size))
            (let end_word := [load_word(expression! { sub((add(head_start, data_end_offset)), 32) }, location)])
            (let padding_size_bits := mul((sub(padded_data_size, data_size)), 8))
            [check_right_padding(
                expression! { padding_size_bits },
                expression! { end_word }
            )]
            (return_val := [copy_data(
                // We do not copy the dynamic size value like we do with strings, so we add 32 bytes
                // to the start and subtract 32 bytes from the size being copied.
                expression! { add(data_start, 32) },
                expression! { sub(data_size, 32) },
                location
            )])
         }
    }
}

pub fn decode_component_string(max_size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_string(max_size, location);
    let max_size = literal_expression! { (max_size) };

    function_definition! {
         function [func_name](head_start, head_offset) -> return_val, data_start_offset, data_end_offset {
            (let head_ptr := add(head_start, head_offset))
            (data_start_offset := [load_word(expression! { head_ptr }, location)])
            (let data_start := add(head_start, data_start_offset))
            (let string_size := [load_word(expression! { data_start }, location)])
            (if (gt(string_size, [max_size])) { [revert_with_invalid_abi_data()] })
            (let data_size := add(string_size, 32))
            (let padded_data_size := ceil32(data_size))
            (data_end_offset := add(data_start_offset, padded_data_size))
            (let end_word := [load_word(expression! { sub((add(head_start, data_end_offset)), 32) }, location)])
            (let padding_size_bits := mul((sub(padded_data_size, data_size)), 8))
            [check_right_padding(
                expression! { padding_size_bits },
                expression! { end_word }
            )]
            (return_val := [copy_data(
                expression! { data_start },
                expression! { data_size },
                location
            )])
         }
    }
}

/// Returns 0 if the value is not padded on the left with zeros.
///
/// `size_bits` refers to the size of the padding in bits.
pub fn is_left_padded() -> yul::Statement {
    function_definition! {
        function is_left_padded(size_bits, val) -> return_val {
            (let bits_shifted := sub(256, size_bits))
            (let shifted_val := shr(bits_shifted, val))
            (return_val := iszero(shifted_val))
        }
    }
}
/// Returns 0 if the value is not padded on the right with zeros.
///
/// `size_bits` refers to the size of the padding in bits.
pub fn is_right_padded() -> yul::Statement {
    function_definition! {
        function is_right_padded(size_bits, val) -> return_val {
            (let bits_shifted := sub(256, size_bits))
            (let shifted_val := shl(bits_shifted, val))
            (return_val := iszero(shifted_val))
        }
    }
}

fn revert_with_invalid_abi_data() -> yul::Statement {
    revert_operations::error_revert_numeric(ERROR_INVALID_ABI_DATA)
}

/// Reverts if the value is not left padded with the given number of bits.
fn check_left_padding(size_bits: yul::Expression, val: yul::Expression) -> yul::Statement {
    statement! {
        if (iszero((is_left_padded([size_bits], [val])))) {
            [revert_with_invalid_abi_data()]
        }
    }
}

/// Reverts if the value is not right padded with the given number of bits.
fn check_right_padding(size_bits: yul::Expression, val: yul::Expression) -> yul::Statement {
    statement! {
        if (iszero((is_right_padded([size_bits], [val])))) {
            [revert_with_invalid_abi_data()]
        }
    }
}

/// Reverts if the integer value does not fit within the given number of bytes.
fn check_int_size(size: usize, val: yul::Expression) -> yul::Statement {
    // the bits to the left of this size should be either all 0s or all 1s
    let size_bits = literal_expression! { (size * 8 - 1) };
    let is_all_0s = expression! { iszero((shr([size_bits.clone()], [val.clone()]))) };
    let is_all_1s = expression! { iszero((shr([size_bits], (not([val]))))) };
    let is_all_0s_or_1s = expression! { or([is_all_0s], [is_all_1s]) };

    statement! {
        if (iszero([is_all_0s_or_1s])) {
            [revert_with_invalid_abi_data()]
        }
    }
}

fn load_word(ptr: yul::Expression, location: AbiDecodeLocation) -> yul::Expression {
    match location {
        AbiDecodeLocation::Memory => expression! { mload([ptr]) },
        AbiDecodeLocation::Calldata => expression! { calldataload([ptr]) },
    }
}

fn copy_data(
    ptr: yul::Expression,
    size: yul::Expression,
    location: AbiDecodeLocation,
) -> yul::Expression {
    match location {
        AbiDecodeLocation::Memory => expression! { mcopym([ptr], [size]) },
        AbiDecodeLocation::Calldata => expression! { ccopym([ptr], [size]) },
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

/// Generates an encoding function for any set of type parameters.
pub fn encode(types: &[AbiType]) -> yul::Statement {
    let func_name = abi_names::encode(types);

    // Create names for each of the values we're encoding.
    let (param_idents, param_exprs) = abi_names::vals("encode", types.len());
    let typed_params: Vec<_> = types.iter().zip(param_exprs).collect();

    // Encode the head section of each component.
    let head_encode_stmts: Vec<_> = typed_params
        .clone()
        .into_iter()
        .map(|(typ, param)| match typ {
            AbiType::StaticArray { inner, size } => encode_static_array(param, inner, *size),
            AbiType::Tuple { components } => encode_tuple(param, components),
            AbiType::Uint { .. } => encode_uint(param),
            AbiType::Int { .. } => encode_uint(param),
            AbiType::Bool => encode_uint(param),
            AbiType::Address => encode_uint(param),
            AbiType::String { .. } => encode_string_head(param),
            AbiType::Bytes { size } => encode_bytes_head(*size),
        })
        .collect();

    // Encode the data section of each component with dynamically-sized data.
    let data_encode_stmts: Vec<_> = typed_params
        .into_iter()
        .filter_map(|(typ, param)| match typ {
            AbiType::String { .. } => Some(encode_string_data(param)),
            AbiType::Bytes { size } => Some(encode_bytes_data(*size, param)),
            _ => None,
        })
        .collect();

    function_definition! {
        function [func_name]([param_idents...]) -> return_ptr {
            // Set the return to the available memory address.
            (return_ptr := avail())
            // The data section begins at the end of the head.
            (let data_offset := [abi_operations::encoding_head_size(types)])
            [head_encode_stmts...]
            [data_encode_stmts...]
        }
    }
}

fn encode_tuple(val: yul::Expression, components: &[AbiType]) -> yul::Statement {
    let tuple_size = components.len() * 32;
    let tuple_size = literal_expression! { (tuple_size) };
    statement! { pop((mcopym([val], [tuple_size]))) }
}

fn encode_uint(val: yul::Expression) -> yul::Statement {
    block_statement! {
        (let ptr := alloc(32))
        (mstore(ptr, [val]))
    }
}

fn encode_string_head(ptr: yul::Expression) -> yul::Statement {
    block_statement! {
        (mstore((alloc(32)), data_offset))
        (let num_bytes := mload([ptr]))
        (let data_size := ceil32((add(32, num_bytes))))
        (data_offset := add(data_offset, data_size))
    }
}

fn encode_string_data(ptr: yul::Expression) -> yul::Statement {
    block_statement! {
        (let num_bytes := mload([ptr.clone()]))
        (let data_size := add(32, num_bytes))
        (let remainder := sub((ceil32(data_size)), data_size))
        (pop((mcopym([ptr], data_size))))
        (pop((alloc(remainder))))
    }
}

fn encode_bytes_head(size: usize) -> yul::Statement {
    block_statement! {
        (mstore((alloc(32)), data_offset))
        (let data_size := [literal_expression! { (ceil_32(32 + size)) }])
        (data_offset := add(data_offset, data_size))
    }
}

fn encode_bytes_data(size: usize, ptr: yul::Expression) -> yul::Statement {
    block_statement! {
        (let num_bytes := [literal_expression! { (size) }])
        (let remainder := [literal_expression! { (ceil_32(size) - size) }])
        (mstore((alloc(32)), num_bytes))
        (pop((mcopym([ptr], num_bytes))))
        (pop((alloc(remainder))))
    }
}

fn encode_static_array(val: yul::Expression, inner: &AbiType, size: usize) -> yul::Statement {
    abi_operations::unpack(
        val,
        literal_expression! { (size) },
        literal_expression! { (inner.packed_size()) },
    )
}
