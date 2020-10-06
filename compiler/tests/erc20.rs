//! Delete this file once the erc20 contract can be compiled and add a new
//! test in evm_contracts that validates the behavior of the erc20 contract.

use fe_parser;
use std::fs;

#[test]
fn parse_erc20() {
    let src =
        fs::read_to_string("tests/fixtures/erc20_token.fe").expect("unable to read fixture file");

    let tokens = fe_parser::get_parse_tokens(&src).expect("failed to get parse tokens");
    fe_parser::parsers::file_input(&tokens[..]).expect("failed to parse erc20");
}
