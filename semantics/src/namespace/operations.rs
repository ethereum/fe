use crate::errors::SemanticError;
use crate::namespace::types::{
    Array,
    Base,
    Map,
    Type,
};

/// Finds the type of an indexed expression.
///
/// e.g. `foo[42]`
pub fn index(value: Type, index: Type) -> Result<Type, SemanticError> {
    match value {
        Type::Array(array) => index_array(array, index),
        Type::Map(map) => index_map(map, index),
        Type::Base(_) => Err(SemanticError::TypeError),
    }
}

fn index_array(array: Array, index: Type) -> Result<Type, SemanticError> {
    if index != Type::Base(Base::U256) {
        return Err(SemanticError::TypeError);
    }

    Ok(Type::Base(array.inner))
}

fn index_map(map: Map, index: Type) -> Result<Type, SemanticError> {
    if index != Type::Base(map.key) {
        return Err(SemanticError::TypeError);
    }

    Ok(*map.value)
}

#[cfg(test)]
mod tests {
    use crate::errors::SemanticError;
    use crate::namespace::operations;
    use crate::namespace::types::{
        Array,
        Base,
        Map,
        Type,
    };
    use rstest::rstest;

    const U256_ARRAY: Type = Type::Array(Array {
        inner: Base::U256,
        dimension: 100,
    });
    const U256: Type = Type::Base(Base::U256);
    const BOOL: Type = Type::Base(Base::Bool);

    fn u256_bool_map() -> Type {
        Type::Map(Map {
            key: Base::U256,
            value: Box::new(Type::Base(Base::Bool)),
        })
    }

    #[rstest(
        value,
        index,
        expected,
        case(U256_ARRAY, U256, U256),
        case(u256_bool_map(), U256, BOOL)
    )]
    fn basic_index(value: Type, index: Type, expected: Type) {
        let actual = operations::index(value, index).expect("failed to get expected type");
        assert_eq!(actual, expected)
    }

    #[rstest(
        value,
        index,
        case(U256_ARRAY, BOOL),
        case(u256_bool_map(), BOOL),
        case(u256_bool_map(), U256_ARRAY)
    )]
    fn type_error_index(value: Type, index: Type) {
        let actual = operations::index(value, index).expect_err("didn't fail");
        assert_eq!(actual, SemanticError::TypeError)
    }
}
