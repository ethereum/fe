use fe_analyzer::namespace::types::{abi_types, AbiDecodeLocation, AbiEncoding, AbiType};
use yultsur::*;

/// Generates an ABI encoding function name for a given set of types.
pub fn encode<T: AbiEncoding>(_types: &[T]) -> yul::Identifier {
    let name = format!("abi_encode_{}", types(&abi_types(_types)));

    identifier! { (name) }
}

pub fn decode_data<T: AbiEncoding>(_types: &[T], location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!(
        "abi_decode_data_{}_{}",
        types(&abi_types(_types)),
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component(typ: &AbiType, location: AbiDecodeLocation) -> yul::Identifier {
    match typ {
        AbiType::Address => decode_component_address(location),
        AbiType::Bool => decode_component_bool(location),
        AbiType::Uint { size } => decode_component_uint(*size, location),
        AbiType::Int { size } => decode_component_int(*size, location),
        AbiType::StaticArray { inner, size } => {
            decode_component_static_array(inner, *size, location)
        }
        AbiType::Tuple { components: elems } => decode_component_tuple(elems, location),
        AbiType::String { max_size } => decode_component_string(*max_size, location),
        AbiType::Bytes { size } => decode_component_bytes(*size, location),
    }
}

pub fn decode_component_uint(size: usize, location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_uint{}_{}",
        size * 8,
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component_int(size: usize, location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_int{}_{}",
        size * 8,
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component_bool(location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!("abi_decode_component_bool_{}", decode_location(location));

    identifier! { (name) }
}

pub fn decode_component_address(location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!("abi_decode_component_address_{}", decode_location(location));

    identifier! { (name) }
}

pub fn decode_component_static_array(
    inner: &AbiType,
    size: usize,
    location: AbiDecodeLocation,
) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_static_array_{}_{}_{}",
        size,
        typ(inner),
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component_tuple(
    components: &[AbiType],
    location: AbiDecodeLocation,
) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_tuple_{}_{}",
        types(components),
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component_bytes(size: usize, location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_bytes_{}_{}",
        size,
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn decode_component_string(max_size: usize, location: AbiDecodeLocation) -> yul::Identifier {
    let name = format!(
        "abi_decode_component_string_{}_{}",
        max_size,
        decode_location(location)
    );

    identifier! { (name) }
}

pub fn vals(prefix: &str, n: usize) -> (Vec<yul::Identifier>, Vec<yul::Expression>) {
    (0..n)
        .into_iter()
        .map(|index| {
            let name = format!("{}_val_{}", prefix, index);
            (identifier! { (name) }, identifier_expression! { (name) })
        })
        .unzip()
}

fn typ(_typ: &AbiType) -> String {
    match _typ {
        AbiType::Uint { size } => format!("uint{}", size * 8),
        AbiType::Int { size } => format!("int{}", size * 8),
        AbiType::Bool => "bool".to_string(),
        AbiType::Address => "address".to_string(),
        AbiType::StaticArray { size, inner } => format!("array_{}_{}", size, typ(inner)),
        AbiType::Tuple { components } => format!("tuple_{}", types(components)),
        AbiType::String { max_size } => format!("string_{}", max_size),
        AbiType::Bytes { size } => format!("bytes_{}", size),
    }
}

fn types(types: &[AbiType]) -> String {
    types.iter().map(typ).collect::<Vec<_>>().join("_")
}

fn decode_location(location: AbiDecodeLocation) -> &'static str {
    match location {
        AbiDecodeLocation::Memory => "mem",
        AbiDecodeLocation::Calldata => "calldata",
    }
}
