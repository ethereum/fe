#![cfg(feature = "solc-backend")]
use evm_runtime::Handler;

use rstest::rstest;
use std::collections::BTreeMap;

use fe_common::utils::keccak;

use fe_compiler_test_utils::{
    self as test_utils, 
    revm::{
        self as revm,
        ethabi,
        address_token,
        Fevm,
        primitive_types::{H160, U256}, 
        revm::{self as evm, EVM, InMemoryDB},
        ContractHarness,
    },
};
use fe_compiler_test_utils::revm::uint_token;
const SOME_ADDRESS: &str = "2012301230123012301230123012301230123002";


pub fn deploy_contract(
    executor: &mut Fevm,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    executor.deploy_contract_from_fixture(
        &format!("features/{}", fixture),
        contract_name,
        init_params,
    )
}

#[test]
fn return_uint_fevm() {
    let mut fevm = Fevm::default();


    let harness = deploy_contract(&mut fevm,"return_u256.fe", "Foo", &[]);
    let call_result = fevm.call_contract(harness.address, "bar", &[]);
    let expected = Some(uint_token(42));
    //assert_eq!(call_result, expected);
    harness.test_function(
        &mut fevm,
        "bar",
        &[],
        Some(&uint_token(42)),
    );
}


#[test]
fn test_balances_revm() {
    let mut fevm = Fevm::default();
    let harness = deploy_contract(&mut fevm,"balances.fe", "Foo", &[]);
    let bob = fevm.create_account_with_balance(0_u64);
     let bob_token = revm::address_token(bob.clone());
     let contract_addr_token = revm::address_token(harness.address.clone());
    harness.test_function(&mut fevm, "my_balance", &[], Some(&uint_token(0)));

    harness.test_function(
        &mut fevm,
        "other_balance",
        &[contract_addr_token.clone()],
        Some(&uint_token(0)),
    );

    harness.test_function(
        &mut fevm,
        "other_balance",
        &[bob_token.clone()],
        Some(&uint_token(0)),
    );
    fevm.fund(&harness.address, 5_u64);
    fevm.fund(&bob, 10_u64);
    assert_eq!(fevm.balance_of(&harness.address), U256::from(5_u64));
    assert_eq!(fevm.balance_of(&bob), U256::from(10_u64));

    harness.test_function(
        &mut fevm,
        "other_balance",
        &[contract_addr_token.clone()],
        Some(&uint_token(5)),
    );

    harness.test_function(
        &mut fevm,
        "other_balance",
        &[bob_token.clone()],
        Some(&uint_token(10)),
    );

    
    // with_executor(&|mut executor| {
    //     let harness = deploy_contract(&mut executor, "balances.fe", "Foo", &[]);
    //     let bob = address("2000000000000000000000000000000000000002");
    //     let bob_token = ethabi::Token::Address(bob);

    //     harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(0)));

    //     harness.test_function(
    //         &mut executor,
    //         "other_balance",
    //         &[ethabi::Token::Address(harness.address)],
    //         Some(&uint_token(0)),
    //     );

    //     harness.test_function(
    //         &mut executor,
    //         "other_balance",
    //         &[bob_token.clone()],
    //         Some(&uint_token(0)),
    //     );

    //     executor.state_mut().deposit(harness.address, U256::from(5));
    //     executor.state_mut().deposit(bob, U256::from(10));

    //     assert_eq!(executor.balance(harness.address), U256::from(5));
    //     assert_eq!(executor.balance(bob), U256::from(10));

    //     harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(5)));

    //     harness.test_function(
    //         &mut executor,
    //         "other_balance",
    //         &[ethabi::Token::Address(harness.address)],
    //         Some(&uint_token(5)),
    //     );

    //     harness.test_function(
    //         &mut executor,
    //         "other_balance",
    //         &[bob_token],
    //         Some(&uint_token(10)),
    //     );
    // })
}