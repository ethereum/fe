use crate::yul::names;
use crate::yul::operations::data as data_operations;
use crate::yul::utils;
use fe_analyzer::namespace::types::{
    AbiArraySize,
    AbiDecodeLocation,
    AbiEncoding,
    AbiType,
    AbiUintSize,
};
use yultsur::*;

/// Returns an expression that encodes the given values and returns a pointer to
/// the encoding.
pub fn encode<T: AbiEncoding>(types: Vec<T>, vals: Vec<yul::Expression>) -> yul::Expression {
    let func_name = names::encode_name(&types);
    expression! { [func_name]([vals...]) }
}

/// Returns an expression that gives size of the encoded values.
pub fn encode_size<T: AbiEncoding>(types: Vec<T>, vals: Vec<yul::Expression>) -> yul::Expression {
    let mut static_size = 0;
    let mut dyn_size = vec![];

    let typed_vals = types.iter().zip(vals);

    for (typ, val) in typed_vals {
        match typ.abi_type() {
            AbiType::Uint { .. } => static_size += 32,
            AbiType::Array { inner, size } => {
                let inner_size = match *inner {
                    AbiType::Uint {
                        size: AbiUintSize { padded_size, .. },
                    } => padded_size,
                    AbiType::Array { .. } => unimplemented!(),
                };
                match size {
                    AbiArraySize::Static { size } => {
                        static_size += utils::ceil_32(inner_size * size)
                    }
                    AbiArraySize::Dynamic => {
                        static_size += 64;
                        let inner_size = literal_expression! { (inner_size) };
                        let array_size = expression! { mload([val]) };
                        dyn_size.push(expression! { ceil32((mul([array_size], [inner_size]))) })
                    }
                }
            }
        }
    }

    let static_size = literal_expression! { (static_size) };
    return expression! { add([static_size], [data_operations::sum(dyn_size)]) };
}

/// Returns a list of expressions that can be used to decode given types.
/// `start` is where the encoding starts and `loc` indicates whether the data is
/// in calldata or memory.
pub fn decode<T: AbiEncoding>(
    types: Vec<T>,
    start: yul::Expression,
    location: AbiDecodeLocation,
) -> Vec<yul::Expression> {
    let offsets = utils::abi_head_offsets(&types).0.into_iter().map(|offset| {
        literal_expression! { (offset) }
    });
    let typed_offsets = types.iter().zip(offsets);

    typed_offsets
        .into_iter()
        .map(|(typ, offset)| {
            let func_name = names::decode_name(typ, location.clone());
            expression! { [func_name]([start.clone()], [offset]) }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::yul::operations::abi::{
        decode,
        encode,
        encode_size,
    };
    use fe_analyzer::namespace::types::{
        AbiDecodeLocation,
        FeString,
        U256,
    };
    use yultsur::*;

    #[test]
    fn test_encode() {
        assert_eq!(
            encode(vec![U256], vec![expression! { 42 }]).to_string(),
            "abi_encode_uint256(42)"
        )
    }

    #[test]
    fn test_encode_size() {
        assert_eq!(
            encode_size(vec![U256], vec![expression! { 42 }]).to_string(),
            "add(32, 0)"
        )
    }

    #[test]
    fn test_decode() {
        assert_eq!(
            decode(
                vec![FeString { max_size: 26 }],
                expression! { 42 },
                AbiDecodeLocation::Calldata
            )[0]
            .to_string(),
            "abi_decode_string26_calldata(42, 0)"
        )
    }
}
