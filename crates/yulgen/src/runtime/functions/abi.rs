use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::utils::ceil_32;
use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, AbiType};
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
pub fn batch_encode<T: AbiEncoding>(batch: Vec<Vec<T>>) -> Vec<yul::Statement> {
    let mut yul_functions: Vec<_> = batch.into_iter().map(encode).collect();
    yul_functions.sort();
    yul_functions.dedup();
    yul_functions
}

/// Creates a batch of decoding function for the given types and decode
/// locations.
///
/// It sorts the functions and removes duplicates.
pub fn batch_decode<T: AbiEncoding + Clone>(
    data_batch: Vec<(Vec<T>, AbiDecodeLocation)>,
) -> Vec<yul::Statement> {
    let component_batch = data_batch
        .iter()
        .fold(vec![], |mut accum, (types, location)| {
            for typ in types.to_owned() {
                accum.push((typ, *location));
            }
            accum
        });

    let data_functions: Vec<_> = data_batch
        .into_iter()
        .map(|(types, location)| decode_data(&types, location))
        .collect();
    let component_functions: Vec<_> = component_batch
        .into_iter()
        .map(|(typ, location)| decode_component(&typ, location))
        .collect();

    let mut yul_functions: Vec<_> = [data_functions, component_functions].concat();
    yul_functions.sort();
    yul_functions.dedup();
    yul_functions
}

/// Creates a function that decodes ABI encoded data.
pub fn decode_data<T: AbiEncoding>(types: &[T], location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_data(&types, location);

    // Create corresponding "val_" and "offset_" names and associate each one
    // with a component type.
    let typed_names: Vec<(&T, String, String)> = types
        .iter()
        .enumerate()
        .map(|(i, typ)| {
            let val = format!("val_{}", i);
            let offset = format!("offset_{}", i);
            (typ, val, offset)
        })
        .collect();

    // Map the return val names to identifiers.
    let return_vals: Vec<_> = typed_names
        .iter()
        .map(|(_, val_name, _)| identifier! { (val_name) })
        .collect();

    // Create declaration statements for each offset.
    let offset_decls: Vec<_> = {
        let mut curr_offset = 0;
        typed_names
            .iter()
            .map(|(typ, _, offset_name)| {
                let offset_ident = identifier! { (offset_name) };
                let offset = literal_expression! { (curr_offset) };
                let offset_decl = statement! { let [offset_ident] := [offset] };
                curr_offset += typ.abi_type().head_size();
                offset_decl
            })
            .collect()
    };

    // Call the component decoding functions for each offset value.
    let decode_stmts: Vec<_> = typed_names
        .iter()
        .map(|(typ, val_name, offset_name)| {
            let val_ident = identifier! { (val_name) };
            let decode_expr = abi_operations::decode_component(
                typ.abi_type(),
                expression! { head_start },
                identifier_expression! { (offset_name) },
                location,
            );
            statement! { [val_ident] := [decode_expr] }
        })
        .collect();

    function_definition! {
         function [func_name](head_start, data_end) -> [return_vals...] {
            [offset_decls...]
            [decode_stmts...]
         }
    }
}

/// Creates a function that decodes a single component in ABI encoded data.
pub fn decode_component<T: AbiEncoding>(typ: &T, location: AbiDecodeLocation) -> yul::Statement {
    match typ.abi_type() {
        AbiType::StaticArray { inner, size } => {
            decode_component_static_array(*inner, size, location)
        }
        AbiType::Tuple { components: elems } => decode_component_tuple(&elems, location),
        AbiType::Uint { size } => decode_component_uint(size, location),
        AbiType::Int { size } => decode_component_int(size, location),
        AbiType::Bool => decode_component_bool(location),
        AbiType::Address => decode_component_address(location),
        AbiType::String { max_size } => decode_component_string(max_size, location),
        AbiType::Bytes { size } => decode_component_bytes(size, location),
    }
}

pub fn decode_component_uint(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_uint(size, location);
    let decode_expr = load_word(expression! { ptr }, location);

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_int(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_int(size, location);
    let decode_expr = load_word(expression! { ptr }, location);

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_bool(location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_bool(location);
    let decode_expr = load_word(expression! { ptr }, location);

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_address(location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_address(location);
    let decode_expr = load_word(expression! { ptr }, location);

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_static_array(
    inner: AbiType,
    array_size: usize,
    location: AbiDecodeLocation,
) -> yul::Statement {
    let func_name = abi_names::decode_component_static_array(&inner, array_size, location);

    let decode_expr = match inner.packed_size() {
        32 => {
            let data_size = literal_expression! { (32 * array_size) };
            copy_data(expression! { ptr }, data_size, location)
        }
        inner_size => abi_operations::pack(
            expression! { ptr },
            literal_expression! { (array_size) },
            literal_expression! { (inner_size) },
            location,
        ),
    };

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_tuple(elems: &[AbiType], location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_tuple(elems, location);
    let tuple_size = literal_expression! { (elems.len() * 32) };
    let decode_expr = copy_data(expression! { ptr }, tuple_size, location);

    function_definition! {
         function [func_name](head_start, offset) -> return_val {
            (let ptr := add(head_start, offset))
            (return_val := [decode_expr])
         }
    }
}

pub fn decode_component_bytes(size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_bytes(size, location);

    function_definition! {
         function [func_name](head_start, head_offset) -> return_val {
            (let head_ptr := add(head_start, head_offset))
            (let data_start_offset := [load_word(expression! { head_ptr }, location)])
            (let data_start := add(32, (add(head_start, data_start_offset))))
            (let data_size := [literal_expression! { (size) }])
            (return_val := [copy_data(
                expression! { data_start },
                expression! { data_size },
                location
            )])
         }
    }
}

pub fn decode_component_string(max_size: usize, location: AbiDecodeLocation) -> yul::Statement {
    let func_name = abi_names::decode_component_string(max_size, location);

    function_definition! {
         function [func_name](head_start, head_offset) -> return_val {
            (let head_ptr := add(head_start, head_offset))
            (let data_start_offset := [load_word(expression! { head_ptr }, location)])
            (let data_start := add(head_start, data_start_offset))
            (let data_size := add(32, [load_word(expression! { data_start }, location)]))
            (return_val := [copy_data(
                expression! { data_start },
                expression! { data_size },
                location
            )])
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

/// Generates an encoding function for any set of type parameters.
pub fn encode<T: AbiEncoding>(types: Vec<T>) -> yul::Statement {
    let func_name = abi_names::encode(&types);

    // Create names for each of the values we're encoding.
    let (param_idents, param_exprs) = abi_names::vals("encode", types.len());
    let typed_params: Vec<_> = types.iter().zip(param_exprs).collect();

    // Encode the head section of each component.
    let head_encode_stmts: Vec<_> = typed_params
        .clone()
        .into_iter()
        .map(|(typ, param)| match typ.abi_type() {
            AbiType::StaticArray { inner, size } => encode_static_array(param, *inner, size),
            AbiType::Tuple { components } => encode_tuple(param, components),
            AbiType::Uint { .. } => encode_uint(param),
            AbiType::Int { .. } => encode_uint(param),
            AbiType::Bool => encode_uint(param),
            AbiType::Address => encode_uint(param),
            AbiType::String { .. } => encode_string_head(param),
            AbiType::Bytes { size } => encode_bytes_head(size),
        })
        .collect();

    // Encode the data section of each component with dynamically-sized data.
    let data_encode_stmts: Vec<_> = typed_params
        .into_iter()
        .filter_map(|(typ, param)| match typ.abi_type() {
            AbiType::String { .. } => Some(encode_string_data(param)),
            AbiType::Bytes { size } => Some(encode_bytes_data(size, param)),
            _ => None,
        })
        .collect();

    function_definition! {
        function [func_name]([param_idents...]) -> return_ptr {
            // Set the return to the available memory address.
            (return_ptr := avail())
            // The data section begins at the end of the head.
            (let data_offset := [abi_operations::encode_head_size(&types)])
            [head_encode_stmts...]
            [data_encode_stmts...]
        }
    }
}

fn encode_tuple(val: yul::Expression, elems: Vec<AbiType>) -> yul::Statement {
    let tuple_size = elems.len() * 32;
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

fn encode_static_array(val: yul::Expression, inner: AbiType, size: usize) -> yul::Statement {
    abi_operations::unpack(
        val,
        literal_expression! { (size) },
        literal_expression! { (inner.packed_size()) },
    )
}
