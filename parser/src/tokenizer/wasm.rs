#![cfg(target_arch = "wasm32")]

use wasm_bindgen::prelude::*;

use crate::tokenizer::tokenize as get_token_vec;

/// Get a JSON serialized tokenization of the Fe source code in `input`.
#[wasm_bindgen]
pub fn tokenize(input: &str) -> String {
    let token_vec = get_token_vec(input).unwrap();

    serde_json::to_string_pretty(&token_vec).unwrap()
}
