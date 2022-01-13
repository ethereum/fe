#![cfg(feature = "solc-backend")]
use evm_runtime::Handler;

use rstest::rstest;
use std::collections::BTreeMap;

use fe_common::utils::keccak;
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{
    self as test_utils, 
    revm::{
        Fevm,
        primitive_types::{H160, U256}, 
        revm::{self as evm, EVM, InMemoryDB},
        ContractHarness,
    },
};

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