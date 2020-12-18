use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use fe_semantics::namespace::types::{
    AbiArraySize,
    AbiEncoding,
    AbiType,
    AbiUintSize,
};

/// Creates a new spanned expression. Useful in cases where an `Expr` is nested
/// within the node of a `Spanned` object.
pub fn spanned_expression<'a>(span: &Span, exp: &fe::Expr<'a>) -> Spanned<fe::Expr<'a>> {
    Spanned {
        node: (*exp).clone(),
        span: (*span).to_owned(),
    }
}

/// Returns the offset at which each head is located in the static section
/// of an encoding and the total size of the static section.
pub fn abi_head_offsets<T: AbiEncoding>(types: &[T]) -> (Vec<usize>, usize) {
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

/// Rounds up to nearest multiple of 32.
pub fn ceil_32(n: usize) -> usize {
    ((n + 31) / 32) * 32
}

#[cfg(test)]
mod tests {
    use crate::yul::utils::abi_head_offsets;
    use fe_semantics::namespace::types::{
        Array,
        Base,
        FeString,
        FixedSize,
        U256,
    };

    #[test]
    fn test_head_offsets() {
        let types = vec![
            FixedSize::Array(Array {
                inner: U256,
                dimension: 42,
            }),
            FixedSize::Base(U256),
            FixedSize::String(FeString { max_size: 26 }),
            FixedSize::Base(Base::Address),
        ];

        assert_eq!(abi_head_offsets(&types), (vec![0, 1344, 1376, 1408], 1440))
    }
}
