#![cfg(feature = "solc-backend")]

use fe_compiler_test_utils::*;

use std::iter;

#[test]
fn guest_book() {
    with_executor(&|mut executor| {
        let mut harness = deploy_contract(
            &mut executor,
            "fixtures/demos/guest_book.fe",
            "GuestBook",
            &[],
        );

        let sender = address_token("1234000000000000000000000000000000005678");
        let bytes = bytes_token(
            iter::repeat("ten bytes.")
                .take(10)
                .collect::<String>()
                .as_str(),
        );

        harness.caller = sender.clone().into_address().unwrap();

        harness.test_function(&mut executor, "sign", &[bytes.clone()], None);

        harness.test_function(&mut executor, "get_msg", &[sender], Some(&bytes));

        harness.events_emitted(executor, &[("Signed", &[bytes])]);
    })
}
