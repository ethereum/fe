#![cfg(feature = "solc-backend")]
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

pub fn deploy_ingot(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    test_utils::deploy_contract_from_ingot(
        executor,
        &format!("ingots/{}/src", fixture),
        contract_name,
        init_params,
    )
}

#[test]
fn test_ingot_with_visibility() {
    with_executor(&|mut executor| {
        let _harness = deploy_ingot(&mut executor, "pub_contract_ingot", "FooBarBing", &[]);
    })
}

#[test]
fn test_ingot_pub_contract() {
    with_executor(&|mut executor| {
        let _harness = deploy_ingot(&mut executor, "visibility_ingot", "Foo", &[]);
    })
}
#[test]
fn test_basic_ingot() {
    with_executor(&|mut executor| {
        let harness = deploy_ingot(&mut executor, "basic_ingot", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "get_my_baz",
            &[],
            Some(&tuple_token(&[bool_token(true), uint_token(26)])),
        );

        harness.test_function(&mut executor, "get_42", &[], Some(&uint_token(42)));
        harness.test_function(&mut executor, "get_26", &[], Some(&uint_token(26)));
        harness.test_function(
            &mut executor,
            "get_my_dyng",
            &[],
            Some(&tuple_token(&[
                address_token("8"),
                uint_token(42),
                int_token(-1),
            ])),
        );
        harness.test_function(&mut executor, "call_on_path", &[], None);
        harness.test_function(
            &mut executor,
            "create_bing_contract",
            &[],
            Some(&uint_token(90)),
        );
    })
}
