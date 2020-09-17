#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;

#[macro_use]
mod utils;

use wasm_bindgen_test::wasm_bindgen_test;

use fe_parser::tokenizer::wasm::tokenize;

fn assert_fixture_is_valid(filename: &str, input: &str, expected_ser: &str) {
    let actual_ser = tokenize(input);

    assert_strings_eq!(
        actual_ser,
        expected_ser,
        "\nTokenizations did not match for {}",
        filename,
    );
}

/// We only
#[wasm_bindgen_test]
fn test_tokenize() {
    do_with_fixtures!(
        assert_fixture_is_valid,
        "fixtures/tokenizer/wasm/basic.py.json",
    );
}
