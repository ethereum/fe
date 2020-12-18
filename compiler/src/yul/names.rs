use fe_semantics::namespace::types::{
    AbiDecodeLocation,
    AbiEncoding,
};
use yultsur::*;

pub fn func_name(name: &str) -> yul::Identifier {
    identifier! { (format!("$${}", name)) }
}

pub fn var_name(name: &str) -> yul::Identifier {
    identifier! { (format!("${}", name)) }
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
#[cfg(test)]
mod tests {
    use crate::yul::names::{
        decode_name,
        encode_name,
    };
    use fe_semantics::namespace::types::{
        AbiDecodeLocation,
        Array,
        Base,
        FeString,
        FixedSize,
        U256,
    };

    #[test]
    fn test_encode_name() {
        assert_eq!(
            encode_name(&vec![
                FixedSize::Base(U256),
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
            decode_name(&U256, AbiDecodeLocation::Calldata).to_string(),
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
}
