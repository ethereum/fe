use crate::names;
use crate::names::abi as abi_names;
use fe_abi::utils as abi_utils;
use fe_analyzer::namespace::types::{AbiEncoding, FixedSize, Struct, U256};
use yultsur::*;

fn selector(name: &str, params: &[FixedSize]) -> yul::Expression {
    let params = params
        .iter()
        .map(|param| param.abi_selector_name())
        .collect::<Vec<String>>();

    literal_expression! {(abi_utils::func_selector(name, &params))}
}

/// Generate a YUL function to revert with the `Error` signature and the
/// given set of params.
/// NOTE: This is currently used for `assert False, "message"` statements which are
/// encoded as `Error(msg="message")`. This will be removed in the future.
pub fn generate_revert_fn_for_assert(params: &[FixedSize]) -> yul::Statement {
    generate_revert_fn("Error", params, params)
}

/// Generate a YUL function to revert with a specific struct used as error data
pub fn generate_struct_revert(val: &Struct) -> yul::Statement {
    let struct_fields = val.get_field_types();
    generate_revert_fn(&val.name, &[FixedSize::Struct(val.clone())], &struct_fields)
}

/// Generate a YUL function to revert with panic codes
pub fn generate_revert_fn_for_panic() -> yul::Statement {
    let selector = selector("Panic", &[FixedSize::Base(U256)]);

    return function_definition! {
        function revert_with_panic(error_code) {
            (let ptr := alloc_mstoren([selector], 4))
            (pop((alloc_mstoren(error_code, 32))))
            (revert(ptr, (add(4, 32))))
        }
    };
}

/// Generate a YUL function to revert with data
pub fn generate_revert_fn(
    name: &str,
    encoding_params: &[FixedSize],
    selector_params: &[FixedSize],
) -> yul::Statement {
    let abi_encode_fn = abi_names::encode(encoding_params);

    let function_name = names::revert_name(name, selector_params);

    let selector = selector(name, &selector_params);

    return function_definition! {
        function [function_name](data_ptr, size) {
            (let ptr := alloc_mstoren([selector], 4))
            (pop(([abi_encode_fn](data_ptr))))
            (revert(ptr, (add(4, size))))
        }
    };
}

/// Return all revert runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![generate_revert_fn_for_panic()]
}
