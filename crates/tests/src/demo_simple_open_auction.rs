#![cfg(feature = "solc-backend")]
use primitive_types::U256;
use fe_compiler_test_utils::*;
use insta::assert_snapshot;

#[test]
fn simple_open_auction() {
    with_executor(&|mut executor| {
        let beneficiary = address_token(DEFAULT_CALLER);
        let alice = address_token("2000000000000000000000000000000000000002");
        let bob = address_token("3000000000000000000000000000000000000003");

        let bidding_time = uint_token_from_dec_str("3600");

        let mut harness = deploy_contract(
            &mut executor,
            "demos/simple_open_auction.fe",
            "SimpleOpenAuction",
            &[bidding_time.clone(), beneficiary.clone()],
        );

        // alice bid first
        harness.caller = alice.clone().into_address().unwrap();
        harness.value = U256::from(10000); 

        harness.test_function(&mut executor, "bid", &[], None);

        

        // bob bid second
        harness.caller = bob.clone().into_address().unwrap();
        harness.value = U256::from(100000); 

        harness.test_function(&mut executor, "bid", &[], None);


        // verify event emitted
        harness.events_emitted(
            executor, 
            &[
                (
                    "HighestBidIncreased",
                    &[
                        alice,
                        uint_token_from_dec_str("10000"),
                    ]
                ),
                (
                    "HighestBidIncreased",
                    &[
                        bob,
                        uint_token_from_dec_str("100000"),
                    ]
                )
            ]
        );

        assert_harness_gas_report!(harness);
    });
}
