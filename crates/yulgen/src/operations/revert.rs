use crate::names;
use crate::types::AbiType;
use yultsur::*;

/// Revert with an error message
pub fn error_revert(typ: &AbiType, msg: yul::Expression) -> yul::Statement {
    revert("Error", typ, msg)
}

/// Revert with a panic code
pub fn panic_revert(val: usize) -> yul::Statement {
    revert(
        "Panic",
        &AbiType::Uint { size: 32 },
        literal_expression! { (val) },
    )
}

/// Revert with a name and a single value
pub fn revert(name: &str, typ: &AbiType, val: yul::Expression) -> yul::Statement {
    let func_name = names::revert(name, typ);
    statement! { [func_name]([val]) }
}
