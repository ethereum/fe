#![cfg(feature = "solc-backend")]

use fe_compiler_test_utils::*;
use insta::assert_snapshot;

#[test]
fn guest_book() {
    with_executor(&|mut executor| {
        let mut harness = deploy_contract(&mut executor, "demos/guest_book.fe", "GuestBook", &[]);

        let sender = address_token("1234000000000000000000000000000000005678");
        let msg = string_token("hello world");

        harness.caller = sender.clone().into_address().unwrap();

        harness.test_function(&mut executor, "sign", &[msg.clone()], None);

        harness.test_function(&mut executor, "get_msg", &[sender], Some(&msg));

        harness.events_emitted(executor, &[("Signed", &[msg])]);

        assert_harness_gas_report!(harness);
    })
}
