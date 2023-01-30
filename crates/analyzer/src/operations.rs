use crate::context::AnalyzerContext;
use crate::errors::{BinaryOperationError, IndexingError};
use crate::namespace::types::{Array, Integer, Map, TraitOrType, Type, TypeDowncast, TypeId};

use crate::traversal::types::{deref_type, try_coerce_type};
use fe_parser::{ast as fe, node::Node};

/// Finds the type of an index operation and checks types.
///
/// e.g. `foo[42]`
pub fn index(
    context: &mut dyn AnalyzerContext,
    value: TypeId,
    indext: TypeId,
    index_expr: &Node<fe::Expr>,
) -> Result<TypeId, IndexingError> {
    match value.typ(context.db()) {
        Type::Array(array) => index_array(context, &array, indext, index_expr),
        Type::Map(map) => index_map(context, &map, indext, index_expr),
        Type::SPtr(inner) => {
            Ok(Type::SPtr(index(context, inner, indext, index_expr)?).id(context.db()))
        }
        Type::Mut(inner) => {
            Ok(Type::Mut(index(context, inner, indext, index_expr)?).id(context.db()))
        }
        Type::SelfType(id) => match id {
            TraitOrType::TypeId(inner) => index(context, inner, indext, index_expr),
            TraitOrType::TraitId(_) => Err(IndexingError::NotSubscriptable),
        },
        Type::Base(_)
        | Type::Tuple(_)
        | Type::String(_)
        | Type::Contract(_)
        | Type::SelfContract(_)
        | Type::Generic(_)
        | Type::Struct(_)
        | Type::Enum(_) => Err(IndexingError::NotSubscriptable),
    }
}

pub fn expected_index_type(context: &mut dyn AnalyzerContext, obj: TypeId) -> Option<TypeId> {
    match obj.typ(context.db()) {
        Type::Array(_) => Some(Type::u256().id(context.db())),
        Type::Map(Map { key, .. }) => Some(key),
        Type::SPtr(inner) | Type::Mut(inner) => expected_index_type(context, inner),
        Type::SelfType(inner) => match inner {
            TraitOrType::TraitId(_) => None,
            TraitOrType::TypeId(id) => expected_index_type(context, id),
        },
        Type::Base(_)
        | Type::Tuple(_)
        | Type::String(_)
        | Type::Contract(_)
        | Type::SelfContract(_)
        | Type::Generic(_)
        | Type::Enum(_)
        | Type::Struct(_) => None,
    }
}

fn index_array(
    context: &mut dyn AnalyzerContext,
    array: &Array,
    index: TypeId,
    index_expr: &Node<fe::Expr>,
) -> Result<TypeId, IndexingError> {
    let u256 = Type::u256().id(context.db());
    if try_coerce_type(context, Some(index_expr), index, u256, false).is_err() {
        return Err(IndexingError::WrongIndexType);
    }

    Ok(array.inner)
}

fn index_map(
    context: &mut dyn AnalyzerContext,
    map: &Map,
    index: TypeId,
    index_expr: &Node<fe::Expr>,
) -> Result<TypeId, IndexingError> {
    let Map { key, value } = map;

    if try_coerce_type(context, Some(index_expr), index, *key, false).is_err() {
        return Err(IndexingError::WrongIndexType);
    }
    Ok(*value)
}

/// Finds the type of a binary operation and checks types.
pub fn bin(
    context: &mut dyn AnalyzerContext,
    left: TypeId,
    left_expr: &Node<fe::Expr>,
    op: fe::BinOperator,
    right: TypeId,
    right_expr: &Node<fe::Expr>,
) -> Result<TypeId, BinaryOperationError> {
    // Add deref coercions, if necessary (this should always succeed).
    let left = deref_type(context, left_expr, left);
    let right = deref_type(context, right_expr, right);

    if let (Some(left_int), Some(right_int)) =
        (left.as_int(context.db()), right.as_int(context.db()))
    {
        let int = match op {
            fe::BinOperator::Add
            | fe::BinOperator::Sub
            | fe::BinOperator::Mult
            | fe::BinOperator::Div
            | fe::BinOperator::Mod => {
                return bin_arithmetic(context, left, right, right_expr);
            }
            fe::BinOperator::Pow => bin_pow(left_int, right_int),
            fe::BinOperator::LShift | fe::BinOperator::RShift => bin_bit_shift(left_int, right_int),
            fe::BinOperator::BitOr | fe::BinOperator::BitXor | fe::BinOperator::BitAnd => {
                bin_bit(left_int, right_int)
            }
        }?;
        Ok(TypeId::int(context.db(), int))
    } else {
        Err(BinaryOperationError::TypesNotNumeric)
    }
}

fn bin_arithmetic(
    context: &mut dyn AnalyzerContext,
    left: TypeId,
    right: TypeId,
    right_expr: &Node<fe::Expr>,
) -> Result<TypeId, BinaryOperationError> {
    // For now, we require that the types be numeric, have the same signedness,
    // and that left.size() >= right.size(). (The rules imposed by try_coerce_type)
    if try_coerce_type(context, Some(right_expr), right, left, false).is_ok() {
        // TODO: loosen up arightmetic type rules.
        // The rules should be:
        // - Any combination of numeric types can be operated on.
        // - If either number is signed, we return a signed type.
        // - The larger type is returned.
        Ok(left)
    } else {
        // The types are not equal. Again, there is no need to be this strict.
        Err(BinaryOperationError::TypesNotCompatible)
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
