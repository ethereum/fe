use fe_semantics::namespace::types::{
    AbiArraySize,
    AbiDecodeLocation,
    AbiEncoding,
    AbiType,
    AbiUintSize,
};
use yultsur::*;

/// Returns the offset at which each head is located in the static section
/// of an encoding and the total size of the static section.
pub fn head_offsets<T: AbiEncoding>(types: &[T]) -> (Vec<usize>, usize) {
    let mut offsets = vec![];
    let mut curr_offset = 0;

    for typ in types {
        offsets.push(curr_offset);

        curr_offset += match typ.abi_type() {
            AbiType::Array {
                size: AbiArraySize::Dynamic { .. },
                ..
            } => 32,
            AbiType::Array {
                size: AbiArraySize::Static { size },
                inner,
            } => match *inner {
                AbiType::Array { .. } => unimplemented!(),
                AbiType::Uint {
                    size: AbiUintSize { padded_size, .. },
                } => ceil_32(padded_size * size),
            },
            AbiType::Uint {
                size: AbiUintSize { padded_size, .. },
            } => padded_size,
        };
    }

    (offsets, curr_offset)
}

/// Generates an ABI encoding function name for a given set of types.
pub fn encode_name<T: AbiEncoding>(types: &[T]) -> yul::Identifier {
    let mut full_name = "abi_encode".to_string();

    for typ in types {
        full_name.push('_');
        full_name.push_str(&typ.abi_safe_name());
    }

    identifier! { (full_name) }
}

/// Generates an ABI decoding function name for a given type and location.
pub fn decode_name<T: AbiEncoding>(typ: &T, location: AbiDecodeLocation) -> yul::Identifier {
    let mut full_name = "abi_decode".to_string();
    let loc = match location {
        AbiDecodeLocation::Memory => "mem",
        AbiDecodeLocation::Calldata => "calldata",
    };
    full_name.push('_');
    full_name.push_str(&typ.abi_safe_name());
    full_name.push('_');
    full_name.push_str(loc);

    identifier! { (full_name) }
}

/// Rounds up to nearest multiple of 32.
pub fn ceil_32(n: usize) -> usize {
    ((n + 31) / 32) * 32
}

#[cfg(test)]
mod tests {
    use crate::yul::abi::utils::{
        decode_name,
        encode_name,
        head_offsets,
    };
    use fe_semantics::namespace::types::{
        AbiDecodeLocation,
        Array,
        Base,
        FeString,
        FixedSize,
    };

    #[test]
    fn test_encode_name() {
        assert_eq!(
            encode_name(&vec![
                FixedSize::Base(Base::U256),
                FixedSize::Array(Array {
                    inner: Base::Byte,
                    dimension: 100
                })
            ])
            .to_string(),
            "abi_encode_uint256_bytes100"
        )
    }

    #[test]
    fn test_decode_name_u256_calldata() {
        assert_eq!(
            decode_name(&Base::U256, AbiDecodeLocation::Calldata).to_string(),
            "abi_decode_uint256_calldata"
        )
    }

    #[test]
    fn test_decode_name() {
        assert_eq!(
            decode_name(&FeString { max_size: 42 }, AbiDecodeLocation::Memory).to_string(),
            "abi_decode_string42_mem"
        )
    }

    #[test]
    fn test_head_offsets() {
        let types = vec![
            FixedSize::Array(Array {
                inner: Base::U256,
                dimension: 42,
            }),
            FixedSize::Base(Base::U256),
            FixedSize::String(FeString { max_size: 26 }),
            FixedSize::Base(Base::Address),
        ];

        assert_eq!(head_offsets(&types), (vec![0, 1344, 1376, 1408], 1440))
    }
}
