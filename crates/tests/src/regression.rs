//! Regression tests for bugs that we find

#![cfg(feature = "solc-backend")]
use insta::assert_snapshot;
use primitive_types::H160;

use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    test_utils::deploy_contract(
        executor,
        &format!("regression/{}", fixture),
        contract_name,
        init_params,
    )
}

pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    test_utils::load_contract(address, &format!("regression/{}", fixture), contract_name)
}

#[test]
fn create_contract_mem() {
    with_executor(&|mut executor| {
        let factory_harness =
            deploy_contract(&mut executor, "create_contract_mem.fe", "FooFactory", &[]);

        let foo_address = factory_harness
            .call_function(&mut executor, "create_foo", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create_contract_mem.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));

        assert_harness_gas_report!(factory_harness);
    })
}
