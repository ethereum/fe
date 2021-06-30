//! Solidity tests that help us prove assumptions about how Solidty handles
//! certain things

#![cfg(feature = "solc-backend")]
use rstest::rstest;

use fe_compiler_test_utils::*;

#[rstest(
    method,
    reason,
    case("revert_me", "Not enough Ether provided."),
    case(
        "revert_with_long_string",
        "A muuuuuch longer reason string that consumes multiple words"
    ),
    case("revert_with_empty_string", "")
)]
fn test_revert_string_reason(method: &str, reason: &str) {
    with_executor(&|mut executor| {
        let harness =
            deploy_solidity_contract(&mut executor, "solidity/revert_test.sol", "Foo", &[]);

        let exit = harness.capture_call(&mut executor, method, &[]);

        let expected_reason = format!("0x{}", hex::encode(encode_error_reason(reason)));
        if let evm::Capture::Exit((evm::ExitReason::Revert(_), output)) = exit {
            assert_eq!(format!("0x{}", hex::encode(&output)), expected_reason);
        } else {
            panic!("failed")
        };
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
    assert_eq!(format!("0x{}", hex::encode(&encoded)), expected_encoding);
}
