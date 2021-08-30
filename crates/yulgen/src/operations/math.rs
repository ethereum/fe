use fe_analyzer::namespace::types::Integer;
use yultsur::*;

use crate::names;

/// Loads a value of the given type from storage.
pub fn adjust_numeric_size(integer: &Integer, value: yul::Expression) -> yul::Expression {
    if integer.size() < 32 {
        expression! { [names::adjust_numeric_size(integer)]([value]) }
    } else {
        value
    }
}
