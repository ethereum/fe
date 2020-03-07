use ethabi;
use evm;
use evm_runtime::Handler;
use primitive_types;
use std::collections::BTreeMap;
use std::fs;
use stringreader::StringReader;
use vyper_compiler as compiler;

type Executor<'a> = evm::executor::StackExecutor<'a, 'a, evm::backend::MemoryBackend<'a>>;

fn with_executor(test: &dyn Fn(Executor)) {
    let vicinity = evm::backend::MemoryVicinity {
        gas_price: u256("0"),
        origin: h160(5),
        chain_id: u256("0"),
        block_hashes: Vec::new(),
        block_number: u256("0"),
        block_coinbase: h160(5),
        block_timestamp: u256("0"),
        block_difficulty: u256("0"),
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
        .expect("Unable to read fixture file.");

    let json_abi = compiler::abi::build(&src).expect("Unable to build ABI");
    let abi = ethabi::Contract::load(StringReader::new(&json_abi)).expect("Unable to load ABI.");

    (
        compiler::evm::compile(&src).expect("Unable to compile to bytecode"),
        abi,
    )
}

fn create_contract(executor: &mut Executor, bytecode: &str) -> primitive_types::H160 {
    if let evm::Capture::Exit(exit) = executor.create(
        h160(4),
        evm_runtime::CreateScheme::Dynamic,
        u256("0"),
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
    let context = evm::Context {
        address: h160(4),
        caller: h160(4),
        apparent_value: u256("0"),
    };

    let input = function.encode_input(&input).unwrap();

    if let evm::Capture::Exit(exit) =
        executor.call(contract_address, None, input, None, false, context)
    {
        return function
            .decode_output(&exit.1)
            .expect("Unable to decode output.");
    }

    panic!("Failed to run function")
}

fn u256(n: &str) -> primitive_types::U256 {
    primitive_types::U256::from_dec_str(n).unwrap()
}

fn h160(b: u8) -> primitive_types::H160 {
    primitive_types::H160::repeat_byte(b)
}

fn u256_abi_token(n: &str) -> ethabi::Token {
    ethabi::Token::Uint(ethabi::Uint::from(u256(n)))
}

fn u256_array_abi_token(a: Vec<&str>) -> ethabi::Token {
    ethabi::Token::FixedArray(a.iter().map(|n| u256_abi_token(n)).collect())
}

#[test]
fn test_evm_sanity() {
    with_executor(&|mut executor| {
        let address = h160(4);
        let amount = u256("1000");

        executor.deposit(address, amount);
        assert_eq!(executor.balance(address), amount);
    })
}

#[test]
fn test_all() {
    test_simple_contract();
    test_return_list();
    test_multi_param();
    //test_guest_book();
    //test_simple_map();
}

fn test_simple_contract() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("simple_contract.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_abi_token("42")],
        );

        assert_eq!(output[0], u256_abi_token("42"))
    })
}

fn test_return_list() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("return_list.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_abi_token("42")],
        );

        assert_eq!(
            output[0],
            u256_array_abi_token(vec!["0", "0", "0", "42", "0"])
        )
    })
}

fn test_multi_param() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("multi_param.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_abi_token("4"), u256_abi_token("42"), u256_abi_token("420")],
        );

        assert_eq!(
            output[0],
            u256_array_abi_token(vec!["4", "42", "420"])
        )
    })
}

fn test_guest_book() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("guest_book.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_abi_token("42")],
        );

        assert_eq!(
            output[0],
            u256_array_abi_token(vec!["0", "0", "0", "42", "0"])
        )
    })
}

fn test_simple_map() {
    with_executor(&|mut executor| {
        let (bytecode, abi) = compile_fixture("simple_map.vy");
        let contract_address = create_contract(&mut executor, &bytecode);

        let bar = &abi.functions["bar"][0];
        let output = run_function(
            &mut executor,
            contract_address,
            bar,
            &[u256_abi_token("42")],
        );

        assert_eq!(
            output[0],
            u256_array_abi_token(vec!["0", "0", "0", "42", "0"])
        )
    })
}
