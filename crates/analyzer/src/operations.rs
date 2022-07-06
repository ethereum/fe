use crate::errors::{BinaryOperationError, IndexingError};
use crate::namespace::types::{Array, Base, Integer, Map, Type, TypeId};
use crate::AnalyzerDb;

use fe_parser::ast as fe;

/// Finds the type of an index operation and checks types.
///
/// e.g. `foo[42]`
pub fn index(db: &dyn AnalyzerDb, value: TypeId, index: TypeId) -> Result<TypeId, IndexingError> {
    match value.typ(db) {
        Type::Array(array) => index_array(db, &array, index),
        Type::Map(map) => index_map(&map, index),
        Type::Base(_)
        | Type::Tuple(_)
        | Type::String(_)
        | Type::Contract(_)
        | Type::SelfContract(_)
        | Type::Generic(_)
        | Type::Struct(_) => Err(IndexingError::NotSubscriptable),
    }
}

fn index_array(db: &dyn AnalyzerDb, array: &Array, index: TypeId) -> Result<TypeId, IndexingError> {
    if index.typ(db) != Type::u256() {
        return Err(IndexingError::WrongIndexType);
    }

    Ok(array.inner)
}

fn index_map(map: &Map, index: TypeId) -> Result<TypeId, IndexingError> {
    let Map { key, value } = map;
    if index != *key {
        return Err(IndexingError::WrongIndexType);
    }
    Ok(*value)
}

/// Finds the type of a binary operation and checks types.
pub fn bin(
    db: &dyn AnalyzerDb,
    left: TypeId,
    op: fe::BinOperator,
    right: TypeId,
) -> Result<TypeId, BinaryOperationError> {
    if let (Type::Base(Base::Numeric(left)), Type::Base(Base::Numeric(right))) =
        (left.typ(db), right.typ(db))
    {
        let int = match op {
            fe::BinOperator::Add
            | fe::BinOperator::Sub
            | fe::BinOperator::Mult
            | fe::BinOperator::Div
            | fe::BinOperator::Mod => bin_arithmetic(left, right),
            fe::BinOperator::Pow => bin_pow(left, right),
            fe::BinOperator::LShift | fe::BinOperator::RShift => bin_bit_shift(left, right),
            fe::BinOperator::BitOr | fe::BinOperator::BitXor | fe::BinOperator::BitAnd => {
                bin_bit(left, right)
            }
        }?;
        Ok(TypeId::int(db, int))
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

fn bin_arithmetic(left: Integer, right: Integer) -> Result<Integer, BinaryOperationError> {
    if left == right {
        // For now, we require that the types be numeric and equal. In the future, we
        // will want to loosen this up such that arithmetic operations can be performed
        // on different types.
        //
        // The rules should be:
        // - Any combination of numeric types can be operated on.
        // - If either number is signed, we return a signed type.
        // - The larger type is returned.
        Ok(left)
    } else {
        // The types are not equal. Again, there is no need to be this strict.
        Err(BinaryOperationError::TypesNotEqual)
    }
}

fn bin_pow(left: Integer, right: Integer) -> Result<Integer, BinaryOperationError> {
    // The exponent is not allowed to be a signed integer. To allow calculations
    // such as -2 ** 3 we allow the right hand side to be an unsigned integer
    // even if the left side is a signed integer. It is allowed as long as the
    // right side is the same size or smaller than the left side (e.g. i16 ** u16
    // but not i16 ** u32). The type of the result will be the type of the left
    // side and under/overflow checks are based on that type.
    if right.is_signed() {
        Err(BinaryOperationError::RightIsSigned)
    } else if left.can_hold(right) {
        Ok(left)
    } else {
        Err(BinaryOperationError::RightTooLarge)
    }
}

fn bin_bit_shift(left: Integer, right: Integer) -> Result<Integer, BinaryOperationError> {
    // The right side must be unsigned.
    if right.is_signed() {
        Err(BinaryOperationError::RightIsSigned)
    } else {
        Ok(left)
    }
}

fn bin_bit(left: Integer, right: Integer) -> Result<Integer, BinaryOperationError> {
    // We require that both numbers be unsigned and equal in size.
    if !left.is_signed() && left == right {
        Ok(left)
    } else {
        Err(BinaryOperationError::NotEqualAndUnsigned)
    }
}
