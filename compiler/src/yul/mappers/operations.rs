use fe_semantics::namespace::types::{
    Base,
    FixedSize,
};
use yultsur::*;

/// Loads a base value in storage.
///
/// The return expression contains a base value.
pub fn sload(base: Base, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (base.size()) };
    expression! { sloadn([sptr], [size]) }
}

/// Copies a fixed size value from storage to memory.
///
/// The return expression contains a memory pointer.
pub fn scopy(fixed_size: FixedSize, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (fixed_size.size()) };
    expression! { scopy([sptr], [size]) }
}
