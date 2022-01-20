use fevm::{Fevm, ContractBuilder, Contract, Caller, Address, U256};

#[allow(dead_code)]
pub fn uint_token(n: u64) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}
#[test]
fn test_get_u256() {
    let mut fevm = Fevm::new();

    let contract = ContractBuilder::new(&fevm)
        .fixture("features/return_u256.fe", "Foo");
    
    let caller = Caller::random();

    fevm.create_account(&caller,1000);
    let contract = contract.deploy(&caller, &[]);

    let call_result = contract.call("bar", &[], &caller);

    assert_eq!(Some(uint_token(42)), call_result);
}