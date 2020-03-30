use ethabi;
use evm;
use evm_runtime::Handler;
use primitive_types::{U256, H160};
use std::collections::BTreeMap;
use std::fs;
use stringreader::StringReader;
use vyper_compiler as compiler;
use std::iter;
use std::str::FromStr;
use vyper_parser::tokenizer::Token;

type Executor<'a> = evm::executor::StackExecutor<'a, 'a, evm::backend::MemoryBackend<'a>>;

fn with_executor(test: &dyn Fn(Executor)) {
    let vicinity = evm::backend::MemoryVicinity {
        gas_price: U256::zero(),
        origin: H160::zero(),
        chain_id: U256::zero(),
        block_hashes: Vec::new(),
        block_number: U256::zero(),
        block_coinbase: H160::zero(),
        block_timestamp: U256::zero(),
        block_difficulty: U256::zero(),
        block_gas_limit: primitive_types::U256::MAX,
    };
    let state: BTreeMap<primitive_types::H160, evm::backend::MemoryAccount> = BTreeMap::new();
    let config = evm::Config::istanbul();

    let backend = evm::backend::MemoryBackend::new(&vicinity, state);
    let executor = evm::executor::StackExecutor::new(&backend, usize::max_value(), &config);

    test(executor);


}

fn compile_fixture(name: &str) -> (String, ethabi::Contract) {
    let src = fs::read_to_string(format!("tests/fixtures/{}", name))
        .expect("Unable to read fixture file");

    let json_abi = compiler::abi::build(&src).expect("Unable to build ABI");
    let abi = ethabi::Contract::load(StringReader::new(&json_abi)).expect("Unable to load ABI");

    (
        compiler::evm::compile(&src).expect("Unable to compile to bytecode"),
        abi,
    )
}

fn create_contract(executor: &mut Executor, bytecode: &str) -> primitive_types::H160 {
    if let evm::Capture::Exit(exit) = executor.create(
        H160::zero(),
        evm_runtime::CreateScheme::Dynamic,
        U256::zero(),
        hex::decode(bytecode).unwrap(),
        None,
    ) {
        return exit.1.expect("No contract address.");
    }

    panic!("Failed to create contract")
}

fn run_function(
    executor: &mut Executor,
    contract_address: primitive_types::H160,
    function: &ethabi::Function,
    input: &[ethabi::Token],
) -> Vec<ethabi::Token> {
    run_function_w_caller(executor, contract_address, H160::zero(), function, input)
}

fn run_function_w_caller(
    executor: &mut Executor,
    contract_address: primitive_types::H160,
    caller: H160,
    function: &ethabi::Function,
    input: &[ethabi::Token],
) -> Vec<ethabi::Token> {
    let context = evm::Context {
        address: H160::zero(),
        caller,
        apparent_value: U256::zero(),
    };

    let input = function
        .encode_input(&input)
        .expect("Unable to encode input");

    if let evm::Capture::Exit(exit) =
        executor.call(contract_address, None, input, None, false, context)
    {
        return function
            .decode_output(&exit.1)
            .expect("Unable to decode output.");
    }

    panic!("Failed to run function")
}

fn u256_token(n: usize) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

fn address_token(s: &str) -> ethabi::Token {
    ethabi::Token::Address(H160::from_str(s).expect("Couldn't create address from string"))
}

fn bytes_token(s: &str) -> ethabi::Token {
    ethabi::Token::FixedBytes(ethabi::FixedBytes::from(s))
}

fn u256_array_token(v: Vec<usize>) -> ethabi::Token {
    ethabi::Token::FixedArray(v.into_iter().map(|n| u256_token(n)).collect())
}

fn address_array_token(v: Vec<&str>) -> ethabi::Token {
    ethabi::Token::FixedArray(v.into_iter().map(|s| address_token(s)).collect())
}

#[test]
fn test_evm_sanity() {
    with_executor(&|mut executor| {
        let address = H160::zero();
        let amount = U256::from(1000);

        executor.deposit(address, amount);
        assert_eq!(executor.balance(address), amount);
    })
}

#[test]
fn return_u256() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("return_u256.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_token(42)],
        );

        assert_eq!(output[0], u256_token(42))
    })
}

#[test]
fn return_array() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("return_array.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_token(42)],
        );

        assert_eq!(
            output[0],
            u256_array_token(vec![0, 0, 0, 42, 0])
        )
    })
}

#[test]
fn multi_param() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("multi_param.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[
                u256_token(4),
                u256_token(42),
                u256_token(420),
            ],
        );

        assert_eq!(output[0], u256_array_token(vec![4, 42, 420]))
    })
}

#[test]
fn u256_u256_map() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("u256_u256_map.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let write_bar = &abi.functions["write_bar"][0];
        let read_bar = &abi.functions["read_bar"][0];

        run_function(
            &mut executor,
            contract_address,
            write_bar,
            &[u256_token(42), u256_token(420)],
        );

        let output = run_function(
            &mut executor,
            contract_address,
            read_bar,
            &[u256_token(42)],
        );

        assert_eq!(output[0], u256_token(420))
    })
}

#[test]
fn address_bytes10_map() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("address_bytes10_map.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let write_bar = &abi.functions["write_bar"][0];
        let read_bar = &abi.functions["read_bar"][0];

        let address1 = address_token("0000000000000000000000000000000000000001");
        let bytes1 = bytes_token("ten bytes1");

        let address2 = address_token("0000000000000000000000000000000000000002");
        let bytes2 = bytes_token("ten bytes2");

        run_function(
            &mut executor,
            contract_address,
            write_bar,
            &[address1.clone(), bytes1.clone()],
        );

        run_function(
            &mut executor,
            contract_address,
            write_bar,
            &[address2.clone(), bytes2.clone()],
        );

        let output1 = run_function(
            &mut executor,
            contract_address,
            read_bar,
            &[address1],
        );

        let output2 = run_function(
            &mut executor,
            contract_address,
            read_bar,
            &[address2],
        );

        assert_eq!(output1[0], bytes1);
        assert_eq!(output2[0], bytes2)
    })
}

#[test]
fn guest_book() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("guest_book.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let sign = &abi.functions["sign"][0];
        let get_msg = &abi.functions["get_msg"][0];

        let sender = H160::from_str("0000000000000000000000000000000000000001").unwrap();
        let bytes = bytes_token(iter::repeat("ten bytes.").take(10).collect::<String>().as_str());

        run_function_w_caller(
            &mut executor,
            contract_address,
            sender.clone(),
            sign,
            &[bytes.clone()],
        );

        let output = run_function(
            &mut executor,
            contract_address,
            get_msg,
            &[ethabi::Token::Address(sender)],
        );

        assert_eq!(output[0], bytes)
    })
}

#[test]
fn return_sender() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("return_sender.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function_w_caller(
            &mut executor,
            contract_address,
            H160::from_str("0000000000000000000000000000000000000001").unwrap(),
            bar,
            &[u256_token(42)],
        );

        assert_eq!(output[0], address_token("0000000000000000000000000000000000000001"))
    })
}

