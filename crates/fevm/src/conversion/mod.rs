
use crate::{U256, H160};
use std::str::FromStr;


#[allow(dead_code)]
pub fn uint_token(n: u64) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

#[allow(dead_code)]
pub fn uint_token_from_dec_str(val: &str) -> ethabi::Token {
    ethabi::Token::Uint(U256::from_dec_str(val).expect("Not a valid dec string"))
}

#[allow(dead_code)]
pub fn address_token(addr: primitive_types::H160) -> ethabi::Token {
    ethabi::Token::Address(addr)
}

#[allow(dead_code)]
pub fn address_token_from_str(s: &str) -> ethabi::Token {
    // left pads to 40 characters
    ethabi::Token::Address(address(&format!("{:0>40}", s)))
}

#[allow(dead_code)]
pub fn string_token(s: &str) -> ethabi::Token {
    ethabi::Token::String(s.to_string())
}

#[allow(dead_code)]
pub fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

#[allow(dead_code)]
pub fn address(s: &str) -> primitive_types::H160 {
    H160::from_str(s).unwrap_or_else(|_| panic!("couldn't create address from: {}", s))
}


#[allow(dead_code)]
pub fn tuple_token(tokens: &[ethabi::Token]) -> ethabi::Token {
    ethabi::Token::Tuple(tokens.to_owned())
}