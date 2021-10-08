use crate::names::abi as abi_names;
use crate::types::AbiType;
use fe_analyzer::namespace::types::Integer;
use yultsur::*;

pub mod abi;

/// Generate a function name to perform checked addition
pub fn checked_add(size: &Integer) -> yul::Identifier {
    identifier! {(format!("checked_add_{}", size.as_ref().to_lowercase()))}
}

/// Generate a function name to perform checked division
pub fn checked_div(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.as_ref()
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_div_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked modulo
pub fn checked_mod(size: &Integer) -> yul::Identifier {
    let sign: &str = if size.is_signed() {
        "signed"
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_mod_{}", sign.to_lowercase()))}
}

/// Generate a function name to perform checked exponentiation
pub fn checked_exp(size: &Integer) -> yul::Identifier {
    identifier! {(format!("checked_exp_{}", size.as_ref().to_lowercase()))}
}

/// Generate a function name to perform checked multiplication
pub fn checked_mul(size: &Integer) -> yul::Identifier {
    identifier! {(format!("checked_mul_{}", size.as_ref().to_lowercase()))}
}

/// Generate a function name to perform checked subtraction
pub fn checked_sub(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.as_ref()
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_sub_{}", size.to_lowercase()))}
}

/// Generate a function name to adjust the size of the integer
pub fn adjust_numeric_size(size: &Integer) -> yul::Identifier {
    identifier! {(format!("adjust_numeric_{}", size.as_ref().to_lowercase()))}
}

/// Generate a safe function name for a user defined function
pub fn func_name(name: &str) -> yul::Identifier {
    identifier! { (format!("$${}", name)) }
}

/// Generate a safe variable name for a user defined function
pub fn var_name(name: &str) -> yul::Identifier {
    identifier! { (format!("${}", name)) }
}

/// Generates a revert function name for a given name and types
pub fn revert(name: &str, typ: &AbiType) -> yul::Identifier {
    let name = format!("revert_with_{}_{}", name, abi_names::typ(typ));

    identifier! { (name) }
}

/// Generates an external call function name for a given type and location.
pub fn contract_call(contract_name: &str, func_name: &str) -> yul::Identifier {
    let name = format!("{}_{}", contract_name, func_name);
    identifier! { (name) }
}

/// Generates a function name for to interact with a certain struct type
pub fn struct_function_name(struct_name: &str, func_name: &str) -> yul::Identifier {
    let name = format!("struct_{}_{}", struct_name, func_name);
    identifier! { (name) }
}

/// Generates a function name for creating a certain struct type
pub fn struct_new_call(struct_name: &str) -> yul::Identifier {
    struct_function_name(struct_name, "new")
}

/// Generates a function name for reading a named property of a certain struct
/// type
pub fn struct_getter_call(struct_name: &str, field_name: &str) -> yul::Identifier {
    struct_function_name(struct_name, &format!("get_{}_ptr", field_name))
}
