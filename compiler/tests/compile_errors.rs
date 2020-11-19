#![cfg(feature = "solc-backend")]

use fe_compiler::errors::CompileError;
use fe_compiler::evm::CompileStage;

use rstest::rstest;
use std::fs;

#[rstest(
    fixture_file,
    error,
    case(
        "continue_without_loop.fe",
        "[Str(\"semantic error: ContinueWithoutLoop\")]"
    ),
    case(
        "continue_without_loop_2.fe",
        "[Str(\"semantic error: ContinueWithoutLoop\")]"
    ),
    case("break_without_loop.fe", "[Str(\"semantic error: BreakWithoutLoop\")]"),
    case(
        "break_without_loop_2.fe",
        "[Str(\"semantic error: BreakWithoutLoop\")]"
    ),
    case(
        "not_in_scope.fe",
        "[Str(\"semantic error: UndefinedValue { value: \\\"y\\\" }\")]"
    ),
    case(
        "not_in_scope_2.fe",
        "[Str(\"semantic error: UndefinedValue { value: \\\"y\\\" }\")]"
    ),
    case("mismatch_return_type.fe", "[Str(\"semantic error: TypeError\")]"),
    case("unexpected_return.fe", "[Str(\"semantic error: TypeError\")]"),
    case("missing_return.fe", "[Str(\"semantic error: MissingReturn\")]"),
    case(
        "missing_return_in_else.fe",
        "[Str(\"semantic error: MissingReturn\")]"
    ),
    case("strict_boolean_if_else.fe", "[Str(\"semantic error: TypeError\")]"),
    case(
        "return_call_to_fn_without_return.fe",
        "[Str(\"semantic error: TypeError\")]"
    ),
    case(
        "return_call_to_fn_with_param_type_mismatch.fe",
        "[Str(\"semantic error: TypeError\")]"
    )
)]
fn test_compile_errors(fixture_file: &str, error: &str) {
    let src = fs::read_to_string(format!("tests/fixtures/compile_errors/{}", fixture_file))
        .expect("Unable to read fixture file");

    match fe_compiler::evm::compile(&src, CompileStage::AllUpToBytecode) {
        Err(CompileError { errors }) => assert_eq!(format!("{:?}", errors), error),
        _ => panic!(
            "Compiling succeeded when it was expected to fail with: {}",
            error
        ),
    }
}
