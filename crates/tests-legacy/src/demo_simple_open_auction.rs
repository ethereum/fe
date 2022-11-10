#![cfg(feature = "solc-backend")]
use evm_runtime::Handler;
use fe_compiler_test_utils::*;
use insta::assert_snapshot;
use primitive_types::{H160, H256, U256};
use std::collections::BTreeMap;

#[test]
fn simple_open_auction() {
    let vicinity = evm::backend::MemoryVicinity {
        gas_price: U256::zero(),
        origin: H160::zero(),
        chain_id: U256::zero(),
        block_hashes: Vec::new(),
        block_number: U256::zero(),
        block_coinbase: H160::zero(),
        block_timestamp: U256::from(10000),
        block_difficulty: U256::zero(),
        block_gas_limit: primitive_types::U256::MAX,
        block_base_fee_per_gas: U256::zero(),
    };
    let state: BTreeMap<primitive_types::H160, evm::backend::MemoryAccount> = BTreeMap::new();
    let backend = evm::backend::MemoryBackend::new(&vicinity, state);

    with_executor_backend(backend, &|mut executor| {
        let beneficiary = address_token(DEFAULT_CALLER);
        let alice = address_token("2000000000000000000000000000000000000002");
        let bob = address_token("3000000000000000000000000000000000000003");
        let charlie = address_token("4000000000000000000000000000000000000004");

        let bidding_time = uint_token_from_dec_str("3600");

        let mut harness = deploy_contract(
            &mut executor,
            "demos/simple_open_auction.fe",
            "SimpleOpenAuction",
            &[bidding_time, beneficiary],
        );

        // alice bid first
        harness.caller = alice.clone().into_address().unwrap();
        harness.value = U256::from(10000);

        harness.test_function(&mut executor, "bid", &[], None);
        executor.state_mut().deposit(harness.address, harness.value);

        // bob bid second
        harness.caller = bob.clone().into_address().unwrap();
        harness.value = U256::from(100000);

        harness.test_function(&mut executor, "bid", &[], None);
        executor.state_mut().deposit(harness.address, harness.value);

        // alice bib again but fail b/c "Bid not high enough"
        harness.caller = alice.clone().into_address().unwrap();
        harness.value = U256::from(10000);

        validate_revert(
            harness.capture_call(&mut executor, "bid", &[]),
            &encode_revert("BidNotHighEnough(uint256)", &[uint_token(100000)]),
        );

        // alice withdraw money
        harness.caller = alice.clone().into_address().unwrap();
        harness.value = U256::zero();
        harness.test_function(&mut executor, "withdraw", &[], Some(&bool_token(true)));

        //charlie withdraw money
        harness.caller = charlie.into_address().unwrap();
        harness.value = U256::zero();
        harness.test_function(&mut executor, "withdraw", &[], Some(&bool_token(true)));

        // call action_end before auction_end_time.
        validate_revert(
            harness.capture_call(&mut executor, "action_end", &[]),
            &encode_revert("AuctionNotYetEnded()", &[]),
        );

        // change auction_end_time to 100.
        // I set auction_end_time because I can't change the block_timestamp.
        // Notes: Data layout of fe is different with solidity
        // https://fe-lang.org/docs/spec/data_layout/storage/constant_size_values_in_storage.html
        executor
            .set_storage(
                harness.address,
                H256::from_slice(&[
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0,
                ]),
                H256::from_slice(&[
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 100,
                ]),
            )
            .unwrap();

        // bid after auction_end_time
        harness.caller = alice.clone().into_address().unwrap();
        harness.value = U256::from(10000);
        validate_revert(
            harness.capture_call(&mut executor, "bid", &[]),
            &encode_revert("AuctionAlreadyEnded()", &[]),
        );

        // action_end test
        harness.test_function(&mut executor, "action_end", &[], None);
        validate_revert(
            harness.capture_call(&mut executor, "action_end", &[]),
            &encode_revert("AuctionEndAlreadyCalled()", &[]),
        );

        // verify event emitted
        harness.events_emitted(
            executor,
            &[
                (
                    "HighestBidIncreased",
                    &[alice, uint_token_from_dec_str("10000")],
                ),
                (
                    "HighestBidIncreased",
                    &[bob.clone(), uint_token_from_dec_str("100000")],
                ),
                ("AuctionEnded", &[bob, uint_token_from_dec_str("100000")]),
            ],
        );

        assert_harness_gas_report!(harness);
    });
}
