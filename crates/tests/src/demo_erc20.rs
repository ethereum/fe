#![cfg(feature = "solc-backend")]

use fe_compiler_test_utils::*;
use insta::assert_snapshot;

#[test]
fn erc20_token() {
    with_executor(&|mut executor| {
        let token_name = string_token("Fe Coin");
        let token_symbol = string_token("fe");

        let mut harness = deploy_contract(
            &mut executor,
            "demos/erc20_token.fe",
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

        // transfer from alice to bob
        harness.test_function(
            &mut executor,
            "transfer",
            &[
                address_token(bob),
                uint_token_from_dec_str("4200000000000000"),
            ],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token(bob)],
            Some(&uint_token_from_dec_str("4200000000000000")),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token(alice)],
            Some(&uint_token_from_dec_str("999999995800000000000000")),
        );

        // approve and transfer
        // alice approves bob to send 50 Fe Coins
        harness.test_function(
            &mut executor,
            "approve",
            &[
                address_token(bob),
                uint_token_from_dec_str("5000000000000000"),
            ],
            Some(&bool_token(true)),
        );
        harness.set_caller(address(bob));
        harness.test_function(
            &mut executor,
            "transferFrom",
            &[
                address_token(alice),
                address_token(james),
                uint_token_from_dec_str("2500000000000000"),
            ],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token(alice)],
            Some(&uint_token_from_dec_str("999999993300000000000000")),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token(james)],
            Some(&uint_token_from_dec_str("2500000000000000")),
        );
        harness.test_function(
            &mut executor,
            "allowance",
            &[address_token(alice), address_token(bob)],
            Some(&uint_token_from_dec_str("2500000000000000")),
        );
        harness.test_function_reverts(
            &mut executor,
            "transferFrom",
            &[
                address_token(alice),
                address_token(bob),
                uint_token_from_dec_str("5000000000000000"),
            ],
            &encoded_panic_assert(),
        );
        harness.test_function(
            &mut executor,
            "transferFrom",
            &[
                address_token(alice),
                address_token(james),
                uint_token_from_dec_str("2000000000000000"),
            ],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token(james)],
            Some(&uint_token_from_dec_str("4500000000000000")),
        );

        // validate events
        harness.events_emitted(
            executor,
            &[
                (
                    "Transfer",
                    &[
                        address_token(alice),
                        address_token(bob),
                        uint_token_from_dec_str("4200000000000000"),
                    ],
                ),
                (
                    "Transfer",
                    &[
                        address_token(alice),
                        address_token(james),
                        uint_token_from_dec_str("2500000000000000"),
                    ],
                ),
                (
                    "Transfer",
                    &[
                        address_token(alice),
                        address_token(james),
                        uint_token_from_dec_str("2000000000000000"),
                    ],
                ),
                (
                    "Approval",
                    &[
                        address_token(alice),
                        address_token(bob),
                        uint_token_from_dec_str("5000000000000000"),
                    ],
                ),
                (
                    "Approval",
                    &[
                        address_token(alice),
                        address_token(bob),
                        uint_token_from_dec_str("2500000000000000"),
                    ],
                ),
                (
                    "Approval",
                    &[
                        address_token(alice),
                        address_token(bob),
                        uint_token_from_dec_str("500000000000000"),
                    ],
                ),
            ],
        );

        assert_harness_gas_report!(harness);
    });
}
