#![cfg(feature = "solc-backend")]
use fe_compiler_test_utils::*;
use serde::{Deserialize, Serialize};
// Create fuzz test for the contracts

#[derive(Serialize, Deserialize, PartialEq, Eq)]
pub struct TokenDetails<'a> {
    pub name: &'a str,
    pub symbol: &'a str,
}

pub fn fuzz_for_erc20(details: TokenDetails) {
    if !details.name.is_empty() {
        with_executor(&|mut executor| {
            let token_name = string_token(details.name);
            let token_symbol = string_token(details.symbol);

            let mut harness = deploy_contract(
                &mut executor,
                "fixtures/fe/erc20.fe",
                "ERC20",
                &[token_name.clone(), token_symbol.clone()],
            );

            let alice = DEFAULT_CALLER;
            let bob = "2000000000000000000000000000000000000002";
            let james = "3000000000000000000000000000000000000003";
            let total_supply = uint_token_from_dec_str("1000000000000000000000000");

            // validate state after init
            // alice starts with 2600 Fe Coins
            harness.test_function(&mut executor, "name", &[], Some(&token_name));
            harness.test_function(&mut executor, "symbol", &[], Some(&token_symbol));
            harness.test_function(&mut executor, "decimals", &[], Some(&uint_token(18)));
            harness.test_function(&mut executor, "totalSupply", &[], Some(&total_supply));
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(alice)],
                Some(&total_supply),
            );
        });
    }
}

pub fn fuzz_erc20(data: &[u8]) {
    serde_json::from_slice(data).ok().map(fuzz_for_erc20);
}
