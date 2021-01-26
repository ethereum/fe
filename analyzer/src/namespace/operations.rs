use crate::errors::SemanticError;
use crate::namespace::types::{
    Array,
    Map,
    Type,
    U256,
};

/// Finds the type of an indexed expression.
///
/// e.g. `foo[42]`
pub fn index(value: Type, index: Type) -> Result<Type, SemanticError> {
    match value {
        Type::Array(array) => index_array(array, index),
        Type::Map(map) => index_map(map, index),
        Type::Base(_) => Err(SemanticError::not_subscriptable()),
        Type::Tuple(_) => Err(SemanticError::not_subscriptable()),
        Type::String(_) => Err(SemanticError::not_subscriptable()),
        Type::Contract(_) => Err(SemanticError::not_subscriptable()),
        Type::Struct(_) => Err(SemanticError::not_subscriptable()),
    }
}

fn index_array(array: Array, index: Type) -> Result<Type, SemanticError> {
    if index != Type::Base(U256) {
        return Err(SemanticError::type_error());
    }

    Ok(Type::Base(array.inner))
}

fn index_map(map: Map, index: Type) -> Result<Type, SemanticError> {
    if index != Type::Base(map.key) {
        return Err(SemanticError::type_error());
    }

    Ok(*map.value)
}

#[cfg(test)]
mod tests {
    use crate::errors::ErrorKind;
    use crate::namespace::operations;
    use crate::namespace::types::{
        Array,
        Base,
        Map,
        Type,
        U256,
    };
    use rstest::rstest;

    const U256_ARRAY_TYPE: Type = Type::Array(Array {
        inner: U256,
        dimension: 100,
    });
    const U256_TYPE: Type = Type::Base(U256);
    const BOOL_TYPE: Type = Type::Base(Base::Bool);

    fn u256_bool_map() -> Type {
        Type::Map(Map {
            key: U256,
            value: Box::new(Type::Base(Base::Bool)),
        })
    }

    #[rstest(
        value,
        index,
        expected,
        case(U256_ARRAY_TYPE, U256_TYPE, U256_TYPE),
        case(u256_bool_map(), U256_TYPE, BOOL_TYPE)
    )]
    fn basic_index(value: Type, index: Type, expected: Type) {
        let actual = operations::index(value, index).expect("failed to get expected type");
        assert_eq!(actual, expected)
    }

    #[rstest(
        value,
        index,
        case(U256_ARRAY_TYPE, BOOL_TYPE),
        case(u256_bool_map(), BOOL_TYPE),
        case(u256_bool_map(), U256_ARRAY_TYPE)
    )]
    fn type_error_index(value: Type, index: Type) {
        let actual = operations::index(value, index).expect_err("didn't fail");
        assert_eq!(actual.kind, ErrorKind::TypeError)
    }
}
