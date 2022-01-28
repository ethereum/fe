#![cfg(feature = "solc-backend")]


use fevm::{Fevm, ContractBuilder, Contract, Caller, Address, U256, conversion::*, AsToken, H160};


#[test]
fn guest_book() {
    let mut fevm = Fevm::new();

    let alice = Caller::random();
    let sender = Caller::random();
    fevm.create_account(&alice, 0_u64);
   
    fevm.create_account(&sender, 1000_u64);

    let guestbook_contract = ContractBuilder::new(&fevm)
    .fixture("demos/guest_book.fe", "GuestBook");
    let guestbook_contract = guestbook_contract.deploy(&alice, &[]);
    let msg = string_token("hello world");

    let sign_result = guestbook_contract.call("sign", &[msg.clone()], &sender);
    assert_eq!(sign_result, None);

    let get_msg_result = guestbook_contract.call("get_msg", &[sender.as_token()], &sender).unwrap();
    assert_eq!(get_msg_result, msg);


    // TO DO: Grab events
    // harness.events_emitted(executor, &[("Signed", &[msg])]);
   
}
