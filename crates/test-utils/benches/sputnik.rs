use evm_runtime::Handler;


use std::collections::BTreeMap;
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};
use fe_common::utils::keccak;
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils, primitive_types::{
    self as primitive_types, H160, U256
}};

const SOME_ADDRESS: &str = "2012301230123012301230123012301230123002";

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    test_utils::deploy_contract(
        executor,
        &format!("features/{}", fixture),
        contract_name,
        init_params,
    )
}

pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    test_utils::load_contract(address, &format!("features/{}", fixture), contract_name)
}

fn test_balances() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "balances.fe", "Foo", &[]);
        let bob = address("2000000000000000000000000000000000000002");
        let bob_token = ethabi::Token::Address(bob);

        harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(0)));

        harness.test_function(
            &mut executor,
            "other_balance",
            &[ethabi::Token::Address(harness.address)],
            Some(&uint_token(0)),
        );

        harness.test_function(
            &mut executor,
            "other_balance",
            &[bob_token.clone()],
            Some(&uint_token(0)),
        );

        executor.state_mut().deposit(harness.address, U256::from(5));
        executor.state_mut().deposit(bob, U256::from(10));

        assert_eq!(executor.balance(harness.address), U256::from(5));
        assert_eq!(executor.balance(bob), U256::from(10));

        harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(5)));

        harness.test_function(
            &mut executor,
            "other_balance",
            &[ethabi::Token::Address(harness.address)],
            Some(&uint_token(5)),
        );

        harness.test_function(
            &mut executor,
            "other_balance",
            &[bob_token],
            Some(&uint_token(10)),
        );
    })
}


pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("balances", |b| b.iter(|| test_balances()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);