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

fn compile_fixture(name: &str) -> (String, String) {
    let src = fs::read_to_string(format!("tests/fixtures/{}", name))
        .expect("Unable to read fixture file.");
    (
        compiler::evm::compile(&src).expect("Unable to compile to bytecode."),
        compiler::abi::build(&src).expect("Unable to build ABI."),
    )
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
fn test_simple_contract() {
    with_executor(&|mut executor| {
        let caller_address = h160(4);
        let (bytecode, abi) = compile_fixture("simple_contract.vy");

        if let evm::Capture::Exit(exit) = executor.create(
            caller_address,
            evm_runtime::CreateScheme::Dynamic,
            u256("0"),
            hex::decode(bytecode).unwrap(),
            None,
        ) {
            let code_address = exit.1.expect("No contract address.");
            let context = evm::Context {
                address: caller_address,
                caller: caller_address,
                apparent_value: u256("0"),
            };
            let contract_abi =
                ethabi::Contract::load(StringReader::new(&abi)).expect("Unable to load ABI.");
            let bar = &contract_abi.functions["bar"][0];

            let params = [u256_abi_token("100")];
            let input = bar.encode_input(&params).unwrap();

            if let evm::Capture::Exit(exit) =
                executor.call(code_address, None, input, None, false, context)
            {
                let output = bar
                    .decode_output(&exit.1)
                    .expect("Unable to decode output.");
                assert_eq!(
                    output[0],
                    u256_abi_token("100"),
                    "bar output does not match."
                )
            }
        } else {
            panic!("Failed to create contract.")
        }
    })
}
