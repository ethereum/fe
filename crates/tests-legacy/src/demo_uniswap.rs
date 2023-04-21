#![cfg(feature = "solc-backend")]

use fe_compiler_test_utils::*;
use insta::assert_snapshot;

#[test]
fn uniswap_contracts() {
    with_executor(&|mut executor| {
        /* SETUP */

        // Create the actors Alice and Bob.
        // Alice starts with all of the token supply (1m each).
        let alice = address_token(DEFAULT_CALLER);
        let bob = address_token("42");

        // Set the names and symbols of our tokens.
        let token0_name = string_token("Fe Coin");
        let token0_symbol = string_token("fe");
        let token1_name = string_token("Maker");
        let token1_symbol = string_token("mkr");

        // Create the token0 contract.
        let token0_harness = deploy_contract(
            &mut executor,
            "demos/erc20_token.fe",
            "ERC20",
            &[token0_name, token0_symbol],
        );

        // Create the token1 contract.
        let mut token1_harness = deploy_contract(
            &mut executor,
            "demos/erc20_token.fe",
            "ERC20",
            &[token1_name, token1_symbol],
        );

        // Alice transfers half of her token1 tokens to Bob (500k)
        token1_harness.test_function(
            &mut executor,
            "transfer",
            &[
                bob.clone(),
                uint_token_from_dec_str("500000000000000000000000"),
            ],
            Some(&bool_token(true)),
        );

        // Set the token addresses for convenience.
        let token0_address = ethabi::Token::Address(token0_harness.address);
        let token1_address = ethabi::Token::Address(token1_harness.address);

        // Deploy the Uniswap pair factory. This is used to create the pair we will
        // test.
        let factory_harness = deploy_contract(
            &mut executor,
            "demos/uniswap.fe",
            "UniswapV2Factory",
            &[address_token("0")],
        );

        // Set the factory address for convenience.
        let factory_address = ethabi::Token::Address(factory_harness.address);

        // Create a token0/token1 pair using the factory.
        let pair_address = factory_harness
            .call_function(
                &mut executor,
                "create_pair",
                &[token0_address.clone(), token1_address.clone()],
            )
            .expect("factory did not return a token");

        // Set the pair address for convenience.
        let pair_harness = load_contract(
            pair_address.clone().into_address().expect("not an address"),
            "demos/uniswap.fe",
            "UniswapV2Pair",
        );

        /* VALIDATE SETUP */

        // Check that the factory address is set correctly
        pair_harness.test_function(&mut executor, "factory", &[], Some(&factory_address));

        // Check that the token0 address is set correctly in the pair contract
        pair_harness.test_function(&mut executor, "token0", &[], Some(&token0_address));

        // Check that the token1 address is set correctly in the pair contract
        pair_harness.test_function(&mut executor, "token1", &[], Some(&token1_address));

        /* ALICE ADDS LIQUIDITY */

        // Alice sends 200 full token0 tokens to the pair for liquidity
        token0_harness.test_function(
            &mut executor,
            "transfer",
            &[
                pair_address.clone(),
                uint_token_from_dec_str("200000000000000000000"),
            ],
            Some(&bool_token(true)),
        );

        // Alice sends 100 full token1 tokens to the pair for liquidity
        token1_harness.test_function(
            &mut executor,
            "transfer",
            &[
                pair_address.clone(),
                uint_token_from_dec_str("100000000000000000000"),
            ],
            Some(&bool_token(true)),
        );

        // Now that Alice has sent tokens to the pair contract, we need to mint her
        // liquidity tokens.
        //
        // Since we have sent 200 of token0 and 100 of token1, the value of token0 is
        // equal to 1/2 that of token1.
        let alices_liquidity = pair_harness
            .call_function(&mut executor, "mint", &[alice.clone()])
            .expect("no return from mint");

        /* VALIDATE LIQUIDITY */

        // Validate that Alice's liquidity token balance is equal to what was returned
        // by `mint`.
        //
        // A portion of the tokens she has added is locked forever to maintain
        // `MINIMUM_LIQUIDITY`, as we will see in the next test.
        pair_harness.test_function(
            &mut executor,
            "balanceOf",
            &[alice.clone()],
            Some(&alices_liquidity),
        );

        // Check that `MINIMUM_LIQUIDITY` is locked at address(0).
        pair_harness.test_function(
            &mut executor,
            "balanceOf",
            &[address_token("0")],
            Some(&uint_token(1000)),
        );

        // Validate reserves.
        pair_harness.test_function(
            &mut executor,
            "get_reserves",
            &[],
            Some(&tuple_token(&[
                uint_token_from_dec_str("200000000000000000000"),
                uint_token_from_dec_str("100000000000000000000"),
                uint_token_from_dec_str("0"),
            ])),
        );

        /* BOB PERFORMS A SWAP */

        // Set Bob as the token1 caller, this is so Bob can perform a swap.
        token1_harness.set_caller(bob.clone().into_address().unwrap());

        // Bob sends 1000 smallest units of token1 to the pair for swapping.
        // token1 is twice as valuable as token0, so we should expect to receive roughly
        // 2000 smallest units of token1 in return.
        token1_harness.test_function(
            &mut executor,
            "transfer",
            &[pair_address.clone(), uint_token(1000)],
            Some(&bool_token(true)),
        );

        // Bob wishes to take 1993 units of token 0 from the pool. The amount received
        // is (2000 - 7). This is accounted for by the .3% swap fee.
        pair_harness.test_function(
            &mut executor,
            "swap",
            &[uint_token(1993), uint_token(0), bob.clone()],
            None,
        );

        /* VALIDATE SWAP */

        // Check that Bob's token0 balance has increased from 0 to 1993 smallest units.
        token0_harness.test_function(
            &mut executor,
            "balanceOf",
            &[bob.clone()],
            Some(&uint_token_from_dec_str("1993")),
        );

        // Validate reserves.
        pair_harness.test_function(
            &mut executor,
            "get_reserves",
            &[],
            Some(&tuple_token(&[
                uint_token_from_dec_str("199999999999999998007"),
                uint_token_from_dec_str("100000000000000001000"),
                uint_token_from_dec_str("0"),
            ])),
        );

        /* ALICE REMOVES LIQUIDITY */

        // Alice sends liquidity back to pair contract.
        pair_harness.test_function(
            &mut executor,
            "transfer",
            &[pair_address.clone(), alices_liquidity],
            Some(&bool_token(true)),
        );

        // Alice burns the liquidity that she has sent back.
        pair_harness.test_function(
            &mut executor,
            "burn",
            &[alice.clone()],
            Some(&tuple_token(&[
                uint_token_from_dec_str("199999999999999996592"),
                uint_token_from_dec_str("100000000000000000292"),
            ])),
        );

        /* VALIDATE LIQUIDITY REMOVAL */

        // Validate reserves.
        pair_harness.test_function(
            &mut executor,
            "get_reserves",
            &[],
            Some(&tuple_token(&[
                uint_token_from_dec_str("1415"),
                uint_token_from_dec_str("708"),
                uint_token_from_dec_str("0"),
            ])),
        );

        /* SANITY CHECK TOKEN BALANCES */

        // Validate that all of the token0 tokens are held between the pair contract and
        // actors.
        //
        // 1993 + 999999999999999999996592 + 1415 = 1e24
        token0_harness.test_function(
            &mut executor,
            "balanceOf",
            &[bob.clone()],
            Some(&uint_token_from_dec_str("1993")),
        );
        token0_harness.test_function(
            &mut executor,
            "balanceOf",
            &[alice.clone()],
            Some(&uint_token_from_dec_str("999999999999999999996592")),
        );
        token0_harness.test_function(
            &mut executor,
            "balanceOf",
            &[pair_address.clone()],
            Some(&uint_token_from_dec_str("1415")),
        );

        // Validate that all of the token1 tokens are held between the pair contract and
        // actors.
        //
        // 499999999999999999999000 + 500000000000000000000292 + 708 = 1e24
        token1_harness.test_function(
            &mut executor,
            "balanceOf",
            &[bob],
            Some(&uint_token_from_dec_str("499999999999999999999000")),
        );
        token1_harness.test_function(
            &mut executor,
            "balanceOf",
            &[alice],
            Some(&uint_token_from_dec_str("500000000000000000000292")),
        );
        token1_harness.test_function(
            &mut executor,
            "balanceOf",
            &[pair_address],
            Some(&uint_token_from_dec_str("708")),
        );

        assert_harness_gas_report!(pair_harness);
        assert_harness_gas_report!(factory_harness);
    });
}
