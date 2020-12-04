#![cfg(feature = "solc-backend")]

use fe_compiler::evm::CompileStage;

use rstest::rstest;
use std::fs;

#[rstest(
    fixture_file,
    expected_error,
    case("continue_without_loop.fe", "ContinueWithoutLoop"),
    case("continue_without_loop_2.fe", "ContinueWithoutLoop"),
    case("break_without_loop.fe", "BreakWithoutLoop"),
    case("break_without_loop_2.fe", "BreakWithoutLoop"),
    case("not_in_scope.fe", "UndefinedValue"),
    case("not_in_scope_2.fe", "UndefinedValue"),
    case("mismatch_return_type.fe", "TypeError"),
    case("unexpected_return.fe", "TypeError"),
    case("missing_return.fe", "MissingReturn"),
    case("missing_return_in_else.fe", "MissingReturn"),
    case("strict_boolean_if_else.fe", "TypeError"),
    case("return_call_to_fn_without_return.fe", "TypeError"),
    case("return_call_to_fn_with_param_type_mismatch.fe", "TypeError"),
    case("return_addition_with_mixed_types.fe", "TypeError"),
    case("return_lt_mixed_types.fe", "TypeError"),
    case("indexed_event.fe", "MoreThanThreeIndexedParams"),
    case("unary_minus_on_bool.fe", "TypeError")
)]
fn test_compile_errors(fixture_file: &str, expected_error: &str) {
    let src = fs::read_to_string(format!("tests/fixtures/compile_errors/{}", fixture_file))
        .expect("Unable to read fixture file");

    match fe_compiler::evm::compile(&src, CompileStage::AllUpToBytecode) {
        Err(compile_error) => assert!(format!("{}", compile_error).contains(expected_error)),
        _ => panic!(
            "Compiling succeeded when it was expected to fail with: {}",
            expected_error
        ),
    }
}
