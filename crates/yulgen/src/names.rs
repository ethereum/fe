use fe_abi::utils as abi_utils;
use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, Integer};
use yultsur::*;

/// Generate a function name to perform checked addition
pub fn checked_add(size: &Integer) -> yul::Identifier {
    let size: &str = size.into();
    identifier! {(format!("checked_add_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked division
pub fn checked_div(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.into()
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
    let size: &str = size.into();
    identifier! {(format!("checked_exp_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked multiplication
pub fn checked_mul(size: &Integer) -> yul::Identifier {
    let size: &str = size.into();
    identifier! {(format!("checked_mul_{}", size.to_lowercase()))}
}

/// Generate a function name to perform checked subtraction
pub fn checked_sub(size: &Integer) -> yul::Identifier {
    let size: &str = if size.is_signed() {
        size.into()
    } else {
        "unsigned"
    };
    identifier! {(format!("checked_sub_{}", size.to_lowercase()))}
}

/// Generate a safe function name for a user defined function
pub fn func_name(name: &str) -> yul::Identifier {
    identifier! { (format!("$${}", name)) }
}

/// Generate a safe variable name for a user defined function
pub fn var_name(name: &str) -> yul::Identifier {
    identifier! { (format!("${}", name)) }
}

/// Generate a revert function name for the name `Error` and a given set of types
pub fn error_revert_name<T: AbiEncoding>(types: &[T]) -> yul::Identifier {
    revert_name("Error", types)
}

/// Generates a revert function name for a given name and types
pub fn revert_name<T: AbiEncoding>(name: &str, types: &[T]) -> yul::Identifier {
    let type_names = types
        .iter()
        .map(|param| param.lower_snake())
        .collect::<Vec<String>>();

    let abi_names = types
        .iter()
        .map(|param| param.abi_selector_name())
        .collect::<Vec<String>>();

    let selector = abi_utils::func_selector(name, &abi_names);

    let name = format!("revert_with_{}_{}", selector, &type_names.join("_"));

    identifier! { (name) }
}

/// Generates an ABI encoding function name for a given set of types.
pub fn encode_name<T: AbiEncoding>(types: &[T]) -> yul::Identifier {
    let type_names = types
        .iter()
        .map(|typ| typ.lower_snake())
        .collect::<Vec<_>>();
    let name = format!("abi_encode_{}", type_names.join("_"));

    identifier! { (name) }
}

/// Generates an ABI decoding function name for a given type and location.
pub fn decode_name<T: AbiEncoding>(typ: &T, location: AbiDecodeLocation) -> yul::Identifier {
    let mut full_name = "abi_decode".to_string();
    let loc = match location {
        AbiDecodeLocation::Memory => "mem",
        AbiDecodeLocation::Calldata => "calldata",
    };
    full_name.push('_');
    full_name.push_str(&typ.lower_snake());
    full_name.push('_');
    full_name.push_str(loc);

    identifier! { (full_name) }
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

#[cfg(test)]
mod tests {
    use crate::names::{decode_name, encode_name};
    use fe_analyzer::namespace::types::{
        AbiDecodeLocation, Array, Base, FeString, FixedSize, U256,
    };

    #[test]
    fn test_encode_name() {
        assert_eq!(
            encode_name(&[
                FixedSize::Base(U256),
                FixedSize::Array(Array {
                    inner: Base::Byte,
                    size: 100
                })
            ])
            .to_string(),
            "abi_encode_u256_array_byte_100"
        )
    }

    #[test]
    fn test_decode_name_u256_calldata() {
        assert_eq!(
            decode_name(&U256, AbiDecodeLocation::Calldata).to_string(),
            "abi_decode_u256_calldata"
        )
    }

    #[test]
    fn test_decode_name() {
        assert_eq!(
            decode_name(&FeString { max_size: 42 }, AbiDecodeLocation::Memory).to_string(),
            "abi_decode_string_42_mem"
        )
    }
}
