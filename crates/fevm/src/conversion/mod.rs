
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
    ethabi::Token::Address(addr.clone())
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


#[allow(dead_code)]
pub fn int_token(val: i64) -> ethabi::Token {
    ethabi::Token::Int(to_2s_complement(val))
}

#[allow(dead_code)]
pub fn to_2s_complement(val: i64) -> U256 {
    // Since this API takes an `i64` we can be sure that the min and max values
    // will never be above what fits the `I256` type which has the same capacity
    // as U256 but splits it so that one half covers numbers above 0 and the
    // other half covers the numbers below 0.

    // Conversion to Two's Complement: https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html

    if val >= 0 {
        U256::from(val)
    } else {
        let positive_val = -val;
        get_2s_complement_for_negative(U256::from(positive_val))
    }
}

#[allow(dead_code)]
pub fn get_2s_complement_for_negative(assume_negative: U256) -> U256 {
    let (negated, _) = assume_negative.overflowing_neg();
    negated + 1
}