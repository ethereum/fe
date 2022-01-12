#![cfg(feature = "solc-backend")]
use evm_runtime::Handler;

use rstest::rstest;
use std::collections::BTreeMap;

use fe_common::utils::keccak;
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{
    self as test_utils, 
    revm::{
        primitive_types::{H160, U256}, 
        revm::{self as evm, EVM, InMemoryDB},
        ContractHarness,
    },
};

const SOME_ADDRESS: &str = "2012301230123012301230123012301230123002";


pub fn deploy_contract(
    executor: &mut EVM<InMemoryDB>,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    revm::deploy_contract(
        executor,
        &format!("features/{}", fixture),
        contract_name,
        init_params,
    )
}

// pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
//     revm::load_contract(address, &format!("features/{}", fixture), contract_name)
// }

#[test]
fn return_array() {
    // with_executor(&|mut executor| {
    //     let harness = deploy_contract(&mut executor, "return_array.fe", "Foo", &[]);

    //     harness.test_function(
    //         &mut executor,
    //         "bar",
    //         &[uint_token(42)],
    //         Some(&uint_array_token(&[0, 0, 0, 42, 0])),
    //     )
    // })
    let mut vm = evm::new();
    vm.database(InMemoryDB::default());

    let harness = deploy_contract(&mut vm, "return_array.fe", "Foo", &[]);
    // println!("Deployed contract address: {:?}", harness.address);
    // assert!(false);
}