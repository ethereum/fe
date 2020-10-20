#![cfg(feature = "solc-backend")]

use fe_compiler::errors::CompileError;

use rstest::rstest;
use std::fs;

#[rstest(
    fixture_file,
    error,
    case(
        "return_call_to_fn_without_return.fe",
        "[Str(\"semantic error: NotAnExpression\")]"
    ),
    case(
        "return_call_to_fn_with_param_type_mismatch.fe",
        "[Str(\"semantic error: TypeError\")]"
    )
)]
fn test_compile_errors(fixture_file: &str, error: &str) {
    let src = fs::read_to_string(format!("tests/fixtures/compile_errors/{}", fixture_file))
        .expect("Unable to read fixture file");

    match fe_compiler::evm::compile(&src) {
        Err(CompileError { errors }) => assert_eq!(format!("{:?}", errors), error),
        _ => panic!(
            "Compiling succeeded when it was expected to fail with: {}",
            error
        ),
    }
}
