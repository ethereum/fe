use fevm::{Fevm, ContractBuilder, Contract, Caller, Address, U256, conversion::*};
use primitive_types::H160;
use std::str::FromStr;


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

#[test]
fn uniswap_fevm() {
    let mut fevm = Fevm::new();

    let alice = Caller::random();
    let bob = Caller::random();
    let deployer = Caller::random();

    fevm.create_account(&alice, 0_u64);
    fevm.create_account(&bob, 0_u64);
    fevm.create_account(&deployer, 2000_u64);

    let token0_name = string_token("Fe Coin");
    let token0_symbol = string_token("fe");
    let token1_name = string_token("Maker");
    let token1_symbol = string_token("mkr");

    let token0_contract = ContractBuilder::new(&fevm)
        .fixture("demos/erc20_token.fe", "ERC20");
    let token0_contract = token0_contract.deploy(&deployer, &[token0_name, token0_symbol]);
    
    let token1_contract = ContractBuilder::new(&fevm)
        .fixture("demos/erc20_token.fe", "ERC20");
    let token1_contract = token1_contract.deploy(&deployer, &[token1_name, token1_symbol]);


    let token0_address = address_token(token0_contract.address.clone().unwrap());
    let token1_address = address_token(token1_contract.address.clone().unwrap());
    token0_contract.call(
        "transfer",
        &[
            alice.as_token(), 
            uint_token_from_dec_str("500000000000000000000000"),
        ],
        &deployer
    );

    token1_contract.call(
        "transfer",
            &[alice.as_token(),
            uint_token_from_dec_str("500000000000000000000000")],
            &deployer
    );

    let balance_alice = token1_contract.call(
        "balanceOf",
        &[alice.as_token()],
        &alice
    ).unwrap();

    assert_eq!(balance_alice, uint_token_from_dec_str("500000000000000000000000"));
    let balance_alice = token0_contract.call(
        "balanceOf",
        &[alice.as_token()],
        &alice
    ).unwrap();

    assert_eq!(balance_alice, uint_token_from_dec_str("500000000000000000000000"));

    let factory_contract = ContractBuilder::new(&fevm)
        .fixture("demos/uniswap.fe", "UniswapV2Factory");
    let factory_contract = factory_contract.deploy(&deployer, &[address_token(H160::default())]);

    let pair_address = factory_contract.call(
        "create_pair", 
        &[token0_address.clone(), token1_address.clone()],
        &deployer
    ).unwrap();
    println!("PAIR contract address: {:?}", pair_address);

    let pair_contract = ContractBuilder::new(&fevm)
        .address(pair_address.clone().into_address().unwrap())
        .fixture( "demos/uniswap.fe", "UniswapV2Pair");

    
    
    let read_factory_ret = pair_contract.call("factory", &[], &deployer).unwrap();

    assert_eq!(read_factory_ret, address_token(factory_contract.address.clone().unwrap()));


    let token0_pair_addr = pair_contract
        .call("token0", &[], &deployer)
        .unwrap();
    assert!(token0_pair_addr == token0_address.clone() || token0_pair_addr == token1_address.clone());

    let token1_pair_addr = pair_contract
        .call("token1", &[], &deployer)
        .unwrap();
        assert!(token1_pair_addr == token1_address.clone() || token0_pair_addr == token1_address.clone());


    // Alice adds liquidity
    let ret = token0_contract
        .call("transfer", &[
            address_token(pair_contract.address.clone().unwrap()),
            uint_token_from_dec_str("200000000000000000000")
        ], &alice)
        .unwrap();
    assert_eq!(ret, bool_token(true));

    let ret = token1_contract
        .call("transfer", &[
            address_token(pair_contract.address.clone().unwrap()),
            uint_token_from_dec_str("100000000000000000000")
        ], &alice)
        .unwrap();
    assert_eq!(ret, bool_token(true));

    // Mint alice liquidity tokens
    // Since we have sent 200 of token0 and 100 of token1,
    // value of token0 is 1/2 that of token1
    let alice_liquidity = pair_contract
        .call("mint", &[alice.as_token()], &bob)
        .unwrap();

    let alice_lp_tkn_balance = pair_contract
        .call("balanceOf",&[alice.as_token()], &bob)
        .unwrap();
    assert_eq!(alice_liquidity, alice_lp_tkn_balance);

    // Check minimum liquidity is locked in 0 address

    let locked_liquidity = pair_contract.call(
        "balanceOf",
            &[address_token_from_str("0")],
            &alice
    ).unwrap();
    assert_eq!(locked_liquidity, uint_token(1000));

    // Validate reserves

    let reserves = pair_contract.call(
        "get_reserves",
        &[],
        &alice
    ).unwrap();
    assert_eq!(reserves, tuple_token(&[
        uint_token_from_dec_str("200000000000000000000"),
        uint_token_from_dec_str("100000000000000000000"),
        uint_token_from_dec_str("1"),
    ]));


    // Give bob some token1 to swap with

    token1_contract.call(
        "transfer",
        &[bob.as_token(), uint_token(1000)],
        &deployer,
    );



    // Bob performs a swap by depositing to 1000 smallest units of token 1 to 
    // the pair contract
    // Since token1 price = 1tk1/2tk0, we should expect to receive
    // roughly 2000 tk0

    token1_contract.call(
        "transfer",
        &[pair_address.clone(), uint_token(1000)],
        &bob
    );

    pair_contract.call(
        "swap",
        &[uint_token(1993), uint_token(0), bob.as_token()],
        &bob
    );

    // Check that bob's token0 balance has increase to 1993 (accounting for 0.3% fee)
    let bob_bal = token0_contract.call(
        "balanceOf",
        &[bob.as_token()],
        &bob
    ).unwrap();

    assert_eq!(bob_bal, uint_token_from_dec_str("1993"));


    // Validate reserves
    let reserves_post_swap = pair_contract.call(
        "get_reserves",
        &[],
        &bob
    ).unwrap();

    assert_eq!(reserves_post_swap, 
        tuple_token(&[
                    uint_token_from_dec_str("199999999999999998007"),
                    uint_token_from_dec_str("100000000000000001000"),
                    uint_token_from_dec_str("1"),
        ])
    );

    // Alice removes Liquidity

    pair_contract.call(
        "transfer",
        &[pair_address.clone(), alice_liquidity],
        &alice
    );

    // Alice burn liquidity she sent back
    let  burned = pair_contract.call(
        "burn",
        &[alice.as_token()],
        &alice
    ).unwrap();
    assert_eq!(
        burned, 
        tuple_token(&[
            uint_token_from_dec_str("199999999999999996592"),
            uint_token_from_dec_str("100000000000000000292"),
            ]
        )
    );

    // Sanity check token balances

    // Validate all of token0 tkns are held between pair contract & actors

    let bob_tkn0 = token0_contract.call(
        "balanceOf",
        &[bob.as_token()],
        &deployer
    ).unwrap();

    assert_eq!(bob_tkn0, uint_token_from_dec_str("1993"));

    let alice_tkn0 = token0_contract.call(
        "balanceOf",
        &[alice.as_token()],
        &deployer
    ).unwrap();

    assert_eq!(alice_tkn0, uint_token_from_dec_str("499999999999999999996592"));

    let pair_tkn0 = token0_contract.call(
        "balanceOf",
        &[pair_address.clone()],
        &deployer
    ).unwrap();

    assert_eq!(pair_tkn0, uint_token_from_dec_str("1415"));

    let deployer_tkn0 = token0_contract.call(
        "balanceOf",
        &[address_token(deployer.0.clone())],
        &deployer
    ).unwrap();
    assert_eq!(deployer_tkn0, uint_token_from_dec_str("500000000000000000000000"));

    

    // Validate token 1 tokens held between pair contract & actors

 let bob_tkn1 = token1_contract.call(
        "balanceOf",
        &[bob.as_token()],
        &deployer
    ).unwrap();

    assert_eq!(bob_tkn1, uint_token_from_dec_str("0"));

    let alice_tkn1 = token1_contract.call(
        "balanceOf",
        &[alice.as_token()],
        &deployer
    ).unwrap();

    assert_eq!(alice_tkn1, uint_token_from_dec_str("500000000000000000000292"));

    let pair_tkn1 = token1_contract.call(
        "balanceOf",
        &[pair_address.clone()],
        &deployer
    ).unwrap();

    assert_eq!(pair_tkn1, uint_token_from_dec_str("708"));

    let deployer_tkn1 = token1_contract.call(
        "balanceOf",
        &[address_token(deployer.0.clone())],
        &deployer
    ).unwrap();
    assert_eq!(deployer_tkn1, uint_token_from_dec_str("499999999999999999999000"));

}