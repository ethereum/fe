#![cfg(feature = "solc-backend")]
use fe_compiler_test_utils::*;
use serde::{Deserialize, Serialize};
// Create fuzz test for the contracts

const ALICE: &str = DEFAULT_CALLER;
const BOB: &str = "2000000000000000000000000000000000000002";
const JAMES: &str = "3000000000000000000000000000000000000003";
const TOTAL_SUPPLY: &str = "1000000000000000000000000";

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
pub struct TokenDetails<'a> {
    pub name: &'a str,
    pub symbol: &'a str,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
pub struct TransferInfo<'a> {
    pub to: &'a str,
    pub value: u128,
}

pub fn erc20_constructor(details: TokenDetails) {
    if !details.name.is_empty() {
        with_executor(&|mut executor| {
            let token_name = string_token(details.name);
            let token_symbol = string_token(details.symbol);

            let harness = deploy_contract(
                &mut executor,
                "fuzz/fixtures/fe/erc20.fe",
                "ERC20",
                &[token_name.clone(), token_symbol.clone()],
            );

            // validate state after init
            // ALICE starts with 2600 Fe Coins
            harness.test_function(&mut executor, "name", &[], Some(&token_name));
            harness.test_function(&mut executor, "symbol", &[], Some(&token_symbol));
            harness.test_function(&mut executor, "decimals", &[], Some(&uint_token(18)));
            harness.test_function(
                &mut executor,
                "totalSupply",
                &[],
                Some(&uint_token_from_dec_str(TOTAL_SUPPLY)),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(ALICE)],
                Some(&uint_token_from_dec_str(TOTAL_SUPPLY)),
            );
        });
    }
}

pub fn erc20_transfer(info: TransferInfo) {
    if info.value >= 1000000000000000000000000 as u128 {
        with_executor(&|mut executor| {
            let token_name = string_token("Fe Coin");
            let token_symbol = string_token("FE");

            let mut harness = deploy_contract(
                &mut executor,
                "fuzz/fixtures/fe/erc20.fe",
                "ERC20",
                &[token_name.clone(), token_symbol.clone()],
            );

            let value = uint_token_from_dec_str(&info.value.to_string());

            harness.test_function(
                &mut executor,
                "transfer",
                &[address_token(info.to), value.clone()],
                Some(&bool_token(true)),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(info.to)],
                Some(&value),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(ALICE)],
                Some(&uint_token_from_dec_str(
                    &(1000000000000000000000000 - info.value).to_string(),
                )),
            );

            // Validate transferFrom and approve.
            harness.set_caller(address(info.to));
            harness.test_function(
                &mut executor,
                "approve",
                &[address_token(ALICE), value.clone()],
                Some(&bool_token(true)),
            );
            harness.set_caller(address(ALICE));
            harness.test_function(
                &mut executor,
                "transferFrom",
                &[
                    address_token(info.to),
                    address_token(JAMES),
                    uint_token_from_dec_str(&(info.value / 2).to_string()),
                ],
                Some(&bool_token(true)),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(info.to)],
                Some(&uint_token_from_dec_str(&(info.value / 2).to_string())),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(JAMES)],
                Some(&uint_token_from_dec_str(&(info.value / 2).to_string())),
            );
            harness.test_function(
                &mut executor,
                "allowance",
                &[address_token(info.to), address_token(ALICE)],
                Some(&uint_token_from_dec_str(&(info.value / 2).to_string())),
            );
            harness.test_function_reverts(
                &mut executor,
                "transferFrom",
                &[address_token(info.to), address_token(BOB), value.clone()],
            );
            harness.test_function(
                &mut executor,
                "transferFrom",
                &[
                    address_token(info.to),
                    address_token(BOB),
                    uint_token_from_dec_str(&(info.value / 4).to_string()),
                ],
                Some(&bool_token(true)),
            );
            harness.test_function(
                &mut executor,
                "balanceOf",
                &[address_token(JAMES)],
                Some(&uint_token_from_dec_str(&(info.value / 4).to_string())),
            );

            // Validate events
            harness.events_emitted(
                executor,
                &[
                    (
                        "Transfer",
                        &[
                            address_token(ALICE),
                            address_token(info.to),
                            value.clone(),
                        ],
                    ),
                    (
                        "Transfer",
                        &[
                            address_token(info.to),
                            address_token(JAMES),
                            uint_token_from_dec_str(&(info.value / 2).to_string()),
                        ],
                    ),
                    (
                        "Transfer",
                        &[
                            address_token(info.to),
                            address_token(BOB),
                            uint_token_from_dec_str(&(info.value / 4).to_string()),
                        ],
                    ),
                    (
                        "Approval",
                        &[
                            address_token(info.to),
                            address_token(ALICE),
                            value,
                        ],
                    ),
                    (
                        "Approval",
                        &[
                            address_token(ALICE),
                            address_token(info.to),
                            uint_token_from_dec_str(&(info.value / 2).to_string()),
                        ],
                    ),
                    (
                        "Approval",
                        &[
                            address_token(ALICE),
                            address_token(info.to),
                            uint_token_from_dec_str(&(info.value / 4).to_string()),
                        ],
                    ),
                ],
            );
        });
    }
}

pub fn fuzz_erc20_constructor(data: &[u8]) {
    let valid_data = data_filter(data);
    bincode::deserialize::<TokenDetails>(&valid_data).ok().map(erc20_constructor);
}

pub fn fuzz_erc20_transfer(data: &[u8]) {
    let valid_data = data_filter(data);
    bincode::deserialize::<TransferInfo>(&valid_data).ok().map(erc20_transfer);
}

pub fn data_filter(data: &[u8]) -> Vec<u8> {
    data.iter()
        .filter(|c| c.is_ascii_alphabetic() || c.is_ascii_digit())
        .map(|c| *c)
        .collect::<Vec<u8>>()
}
