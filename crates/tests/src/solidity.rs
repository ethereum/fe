//! Solidity tests that help us prove assumptions about how Solidty handles
//! certain things

#![cfg(feature = "solc-backend")]
use rstest::rstest;

use fe_compiler_test_utils::*;

#[rstest(
    method,
    params,
    reason,
    case(
        "revert_bare",
        &[],
        &[]
    ),
    case("revert_me", &[], &encode_error_reason("Not enough Ether provided.")),
    case(
        "revert_with_long_string",
        &[],
        &encode_error_reason("A muuuuuch longer reason string that consumes multiple words")
    ),
    case("revert_with_empty_string",&[], &encode_error_reason("")),
    case(
        "revert_with_string_error",
        &[],
        &encode_revert("StringError(string)", &[string_token("Not enough Ether provided.")])
    ),
    case(
        "revert_with_u256_error",
        &[],
        &encode_revert("U256Error(uint256)", &[uint_token(100)])
    ),
    case(
        "revert_with_i256_error",
        &[],
        &encode_revert("I256Error(int256)", &[int_token(-100)])
    ),
    case(
        "revert_with_u8_error",
        &[],
        &encode_revert("U8Error(uint8)", &[uint_token(100)])
    ),
    case(
        "revert_with_two_u256_error",
        &[],
        &encode_revert("TwoU256Error(uint256,uint256)", &[uint_token(100), uint_token(100)])
    ),
    case(
        "revert_with_struct_error",
        &[],
        &encode_revert("StructError((uint256,int256,bool))", &[uint_token(100), int_token(-100), bool_token(true)])
    ),
    case(
        "panic_divide_by_zero",
        &[uint_token(1), uint_token(0)],
        &encode_revert("Panic(uint256)", &[uint_token(0x12)])
    ),
    case(
        "panic_assert",
        &[],
        &encode_revert("Panic(uint256)", &[uint_token(0x01)])
    ),
)]
fn test_revert_errors(method: &str, params: &[ethabi::Token], reason: &[u8]) {
    with_executor(&|mut executor| {
        let harness =
            deploy_solidity_contract(&mut executor, "solidity/revert_test.sol", "Foo", &[], false);

        let exit = harness.capture_call(&mut executor, method, params);

        validate_revert(exit, reason);
    })
}

#[rstest(reason_str, expected_encoding,
    case("Not enough Ether provided.", "0x08c379a00000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000001a4e6f7420656e6f7567682045746865722070726f76696465642e000000000000"),
    case("A muuuuuch longer reason string that consumes multiple words", "0x08c379a00000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003c41206d75757575756368206c6f6e67657220726561736f6e20737472696e67207468617420636f6e73756d6573206d756c7469706c6520776f72647300000000"),
    case("", "0x08c379a000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000"),
    case("foo", "0x08c379a000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003666f6f0000000000000000000000000000000000000000000000000000000000"),
)]
fn test_revert_reason_encoding(reason_str: &str, expected_encoding: &str) {
    let encoded = encode_error_reason(reason_str);
    assert_eq!(format!("0x{}", hex::encode(encoded)), expected_encoding);
}

#[rstest(
    method,
    params,
    expected,
    case("bar", &[int_token(-10)], Some(int_array_token(&[-10]))),
)]
fn signext_int_array(method: &str, params: &[ethabi::Token], expected: Option<ethabi::Token>) {
    with_executor(&|mut executor| {
        let harness =
            deploy_solidity_contract(&mut executor, "solidity/arrays.sol", "Foo", &[], true);

        harness.test_function(&mut executor, method, params, expected.as_ref());
    })
}
