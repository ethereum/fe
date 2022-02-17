use crate::names;
use crate::operations::abi as abi_operations;
use crate::types::AbiType;
use yultsur::*;

/// Generate a YUL function to revert with the `Error` signature and the
/// given string.
///
/// NOTE: This is currently used for `assert False, "message"` statements which are
/// encoded as `Error(msg="message")`. This will be removed in the future.
pub fn error_revert(typ: &AbiType) -> yul::Statement {
    revert("Error", typ)
}

/// Generate a YUL function to revert with an error code.
pub fn error_revert_numeric() -> yul::Statement {
    revert("Error", &AbiType::Uint { size: 32 })
}

/// Generate a YUL function to revert with a panic code.
pub fn panic_revert() -> yul::Statement {
    revert("Panic", &AbiType::Uint { size: 32 })
}

/// Generate a YUL function to revert with any signature name and type.
/// Note: The parentheses on a tuple are removed in the selector preimage.
pub fn revert(name: &str, typ: &AbiType) -> yul::Statement {
    let func_name = names::revert(name, typ);
    // the selector parens around a tuple are removed for the selector preimage
    // e.g. we use `MyError(bool, address)` instead of `MyError((bool, address))`
    let selector = {
        let selector_params = match typ.clone() {
            AbiType::Tuple { components } => components,
            typ => vec![typ],
        };
        let selector = fe_abi::utils::func_selector(
            name,
            &selector_params
                .iter()
                .map(AbiType::selector_name)
                .collect::<Vec<_>>(),
        );
        literal_expression! { (selector) }
    };
    let val_expr = vec![expression! { val }];
    let encoding_size = abi_operations::encoding_size(&[typ.to_owned()], &val_expr);
    let encode_val = abi_operations::encode(&[typ.to_owned()], val_expr);

    function_definition! {
        function [func_name](val) {
            (let ptr := alloc_mstoren([selector], 4))
            (pop([encode_val]))
            (revert(ptr, (add(4, [encoding_size]))))
        }
    }
}

/// Return all revert functions used by default.
pub fn all() -> Vec<yul::Statement> {
    vec![panic_revert(), error_revert_numeric()]
}
