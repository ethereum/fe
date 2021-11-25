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
        &format!("ingots/{}", fixture),
        contract_name,
        init_params,
    )
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
    })
}
