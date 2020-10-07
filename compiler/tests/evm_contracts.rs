#![cfg(feature = "solc-backend")]
use ethabi;
use evm;

use evm_runtime::Handler;
use fe_compiler as compiler;
use primitive_types::{
    H160,
    U256,
};
use rstest::rstest;
use std::collections::BTreeMap;
use std::fs;
use std::iter;
use std::str::FromStr;
use stringreader::StringReader;

type Executor<'a> = evm::executor::StackExecutor<'a, 'a, evm::backend::MemoryBackend<'a>>;

struct ContractHarness {
    address: H160,
    abi: ethabi::Contract,
    pub caller: primitive_types::H160,
}

impl ContractHarness {
    fn new(address: H160, abi: ethabi::Contract) -> Self {
        ContractHarness {
            address,
            abi,
            caller: H160::zero(),
        }
    }

    pub fn capture_call(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &Vec<ethabi::Token>,
    ) -> evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible> {
        let function = &self.abi.functions[name][0];

        let context = evm::Context {
            address: H160::zero(),
            caller: self.caller.clone(),
            apparent_value: U256::zero(),
        };

        let input = function
            .encode_input(input.as_slice())
            .expect("Unable to encode input");

        executor.call(self.address.clone(), None, input, None, false, context)
    }

    pub fn test_function(
        &self,
        executor: &mut Executor,
        name: &str,
        input: Vec<ethabi::Token>,
        output: Option<ethabi::Token>,
    ) {
        let function = &self.abi.functions[name][0];

        if let evm::Capture::Exit(exit) = self.capture_call(executor, name, &input) {
            if let Some(output) = output {
                let actual_output = &function
                    .decode_output(&exit.1)
                    .expect("Unable to decode output.")[0];

                assert_eq!(&output, actual_output)
            }
        } else {
            panic!("Failed to run function")
        }
    }

    // Executor must be passed by value to get emitted events.
    pub fn event_emitted(&self, executor: Executor, name: &str, output: Vec<ethabi::Token>) {
        let raw_logs = executor
            .deconstruct()
            .1
            .into_iter()
            .map(|l| ethabi::RawLog::from((l.topics, l.data)))
            .collect::<Vec<ethabi::RawLog>>();

        let event = self
            .abi
            .events()
            .find(|e| e.name.eq(name))
            .expect("Unable to find event for name");

        let mut values = None;
        for raw_log in raw_logs {
            if let Ok(log) = event.parse_log(raw_log) {
                values = Some(
                    log.params
                        .into_iter()
                        .map(|p| p.value)
                        .collect::<Vec<ethabi::Token>>(),
                );
            }
        }

        if let Some(values) = values {
            assert_eq!(values, output)
        } else {
            panic!("No logs for event")
        }
    }
}

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

    test(executor)
}

fn deploy_contract(executor: &mut Executor, fixture: &str, name: &str) -> ContractHarness {
    let src = fs::read_to_string(format!("tests/fixtures/{}", fixture))
        .expect("Unable to read fixture file");

    let output = compiler::evm::compile(&src).expect("Unable to compile to bytecode");
    let json_abi = compiler::abi::build(&src)
        .expect("Unable to build the module ABIs")
        .contracts[name]
        .json()
        .expect("Unable to serialize the contract ABI.");

    let abi = ethabi::Contract::load(StringReader::new(&json_abi)).expect("Unable to load the ABI");

    if let evm::Capture::Exit(exit) = executor.create(
        H160::zero(),
        evm_runtime::CreateScheme::Dynamic,
        U256::zero(),
        hex::decode(output.bytecode).unwrap(),
        None,
    ) {
        return ContractHarness::new(exit.1.expect("Unable to retrieve contract address"), abi);
    }

    panic!("Failed to create contract")
}

fn u256_token(n: usize) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

fn address_token(s: &str) -> ethabi::Token {
    ethabi::Token::Address(H160::from_str(s).expect("Couldn't create address from string"))
}

fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

fn bytes_token(s: &str) -> ethabi::Token {
    ethabi::Token::FixedBytes(ethabi::FixedBytes::from(s))
}

fn u256_array_token(v: Vec<usize>) -> ethabi::Token {
    ethabi::Token::FixedArray(v.into_iter().map(|n| u256_token(n)).collect())
}

#[test]
fn evm_sanity() {
    with_executor(&|mut executor| {
        let address = H160::zero();
        let amount = U256::from(1000);

        executor.deposit(address, amount);
        assert_eq!(executor.balance(address), amount);
    })
}

#[rstest(fixture_file, input, expected,
    case("return_u256.fe", vec![], Some(u256_token(42))),
    case("return_identity_u256.fe", vec![42], Some(u256_token(42))),
    // binary operators
    case("return_addition_u256.fe", vec![42, 42], Some(u256_token(84))),
    case("return_subtraction_u256.fe", vec![42, 42], Some(u256_token(0))),
    case("return_multiplication_u256.fe", vec![42, 42], Some(u256_token(1764))),
    case("return_division_u256.fe", vec![42, 42], Some(u256_token(1))),
    case("return_pow_u256.fe", vec![2, 0], Some(u256_token(1))),
    case("return_pow_u256.fe", vec![2, 4], Some(u256_token(16))),
    case("return_mod_u256.fe", vec![5, 0], Some(u256_token(0))),
    case("return_mod_u256.fe", vec![5, 2], Some(u256_token(1))),
    case("return_mod_u256.fe", vec![5, 3], Some(u256_token(2))),
    case("return_mod_u256.fe", vec![5, 5], Some(u256_token(0))),
    case("return_bitwiseand_u256.fe", vec![12, 25], Some(u256_token(8))),
    case("return_bitwiseor_u256.fe", vec![12, 25], Some(u256_token(29))),
    case("return_bitwisexor_u256.fe", vec![12, 25], Some(u256_token(21))),
    case("return_bitwiseshl_u256.fe", vec![212, 0], Some(u256_token(212))),
    case("return_bitwiseshl_u256.fe", vec![212, 1], Some(u256_token(424))),
    case("return_bitwiseshr_u256.fe", vec![212, 0], Some(u256_token(212))),
    case("return_bitwiseshr_u256.fe", vec![212, 1], Some(u256_token(106))),
    // comparision operators
    case("return_eq_u256.fe", vec![1, 1], Some(bool_token(true))),
    case("return_eq_u256.fe", vec![1, 2], Some(bool_token(false))),
    case("return_noteq_u256.fe", vec![1, 1], Some(bool_token(false))),
    case("return_noteq_u256.fe", vec![1, 2], Some(bool_token(true))),
    case("return_lt_u256.fe", vec![1, 2], Some(bool_token(true))),
    case("return_lt_u256.fe", vec![1, 1], Some(bool_token(false))),
    case("return_lt_u256.fe", vec![2, 1], Some(bool_token(false))),
    case("return_lte_u256.fe", vec![1, 2], Some(bool_token(true))),
    case("return_lte_u256.fe", vec![1, 1], Some(bool_token(true))),
    case("return_lte_u256.fe", vec![2, 1], Some(bool_token(false))),
    case("return_gt_u256.fe", vec![2, 1], Some(bool_token(true))),
    case("return_gt_u256.fe", vec![1, 1], Some(bool_token(false))),
    case("return_gt_u256.fe", vec![1, 2], Some(bool_token(false))),
    case("return_gte_u256.fe", vec![2, 1], Some(bool_token(true))),
    case("return_gte_u256.fe", vec![1, 1], Some(bool_token(true))),
    case("return_gte_u256.fe", vec![1, 2], Some(bool_token(false))),
)]
fn test_method_return(fixture_file: &str, input: Vec<usize>, expected: Option<ethabi::Token>) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo");
        harness.test_function(
            &mut executor,
            "bar",
            input
                .clone()
                .into_iter()
                .map(|val| u256_token(val))
                .collect(),
            expected.clone(),
        )
    })
}

#[test]
fn return_array() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_array.fe", "Foo");

        harness.test_function(
            &mut executor,
            "bar",
            vec![u256_token(42)],
            Some(u256_array_token(vec![0, 0, 0, 42, 0])),
        )
    })
}

#[test]
fn multi_param() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "multi_param.fe", "Foo");

        harness.test_function(
            &mut executor,
            "bar",
            vec![u256_token(4), u256_token(42), u256_token(420)],
            Some(u256_array_token(vec![4, 42, 420])),
        )
    })
}

#[test]
fn u256_u256_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "u256_u256_map.fe", "Foo");

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![u256_token(4), u256_token(42)],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![u256_token(420), u256_token(12)],
            None,
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            vec![u256_token(4)],
            Some(u256_token(42)),
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            vec![u256_token(420)],
            Some(u256_token(12)),
        );
    })
}

#[test]
fn address_bytes10_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "address_bytes10_map.fe", "Foo");

        let address1 = address_token("0000000000000000000000000000000000000001");
        let bytes1 = bytes_token("ten bytes1");

        let address2 = address_token("0000000000000000000000000000000000000002");
        let bytes2 = bytes_token("ten bytes2");

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![address1.clone(), bytes1.clone()],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![address2.clone(), bytes2.clone()],
            None,
        );

        harness.test_function(&mut executor, "read_bar", vec![address1], Some(bytes1));

        harness.test_function(&mut executor, "read_bar", vec![address2], Some(bytes2));
    })
}

#[test]
fn guest_book() {
    with_executor(&|mut executor| {
        let mut harness = deploy_contract(&mut executor, "guest_book.fe", "GuestBook");

        let sender = address_token("1234000000000000000000000000000000005678");
        let bytes = bytes_token(
            iter::repeat("ten bytes.")
                .take(10)
                .collect::<String>()
                .as_str(),
        );

        harness.caller = sender.clone().to_address().unwrap();

        harness.test_function(&mut executor, "sign", vec![bytes.clone()], None);

        harness.test_function(&mut executor, "get_msg", vec![sender], Some(bytes.clone()));

        harness.event_emitted(executor, "Signed", vec![bytes]);
    })
}

#[test]
fn return_sender() {
    with_executor(&|mut executor| {
        let mut harness = deploy_contract(&mut executor, "return_sender.fe", "Foo");

        let sender = address_token("1234000000000000000000000000000000005678");

        harness.caller = sender.clone().to_address().unwrap();

        harness.test_function(
            &mut executor,
            "bar",
            // FIXME: There's an issue parsing functions with 0 params. This is here to mitigate
            // that failure.
            vec![u256_token(42)],
            Some(sender),
        );
    })
}
