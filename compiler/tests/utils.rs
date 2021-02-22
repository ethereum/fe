#![cfg(feature = "solc-backend")]
use evm_runtime::{
    ExitReason,
    Handler,
};
use fe_compiler as compiler;
use primitive_types::{
    H160,
    U256,
};
use std::collections::BTreeMap;
use std::fs;
use std::str::FromStr;
use stringreader::StringReader;
use yultsur::*;

pub trait ToBeBytes {
    fn to_be_bytes(&self) -> [u8; 32];
}

impl ToBeBytes for U256 {
    fn to_be_bytes(&self) -> [u8; 32] {
        let mut input_bytes: [u8; 32] = [0; 32];
        self.to_big_endian(&mut input_bytes);
        input_bytes
    }
}

#[allow(dead_code)]
pub type Executor<'a> = evm::executor::StackExecutor<'a, 'a, evm::backend::MemoryBackend<'a>>;

#[allow(dead_code)]
pub const DEFAULT_CALLER: &str = "1000000000000000000000000000000000000001";

#[allow(dead_code)]
pub struct ContractHarness {
    pub address: H160,
    pub abi: ethabi::Contract,
    pub caller: H160,
    pub value: U256,
}

#[allow(dead_code)]
impl ContractHarness {
    fn new(contract_address: H160, abi: ethabi::Contract) -> Self {
        let caller = address(DEFAULT_CALLER);

        ContractHarness {
            address: contract_address,
            abi,
            caller,
            value: U256::zero(),
        }
    }

    pub fn capture_call(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) -> evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible> {
        let function = &self.abi.functions[name][0];

        let context = evm::Context {
            address: self.address,
            caller: self.caller,
            apparent_value: self.value,
        };

        let input = function
            .encode_input(input)
            .expect("Unable to encode input");

        executor.call(self.address, None, input, None, false, context)
    }

    pub fn test_function(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
        output: Option<&ethabi::Token>,
    ) {
        let actual_output = self.call_function(executor, name, input);
        assert_eq!(output.map(|token| token.to_owned()), actual_output)
    }

    pub fn call_function(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) -> Option<ethabi::Token> {
        let function = &self.abi.functions[name][0];

        match self.capture_call(executor, name, &input) {
            evm::Capture::Exit((ExitReason::Succeed(_), output)) => {
                let output = function
                    .decode_output(&output)
                    .expect(&format!("unable to decode output: {:?}", &output))
                    .pop();
                return output;
            }
            evm::Capture::Exit((reason, _)) => panic!("failed to run \"{}\": {:?}", name, reason),
            _ => panic!("trap"),
        }
    }

    pub fn test_function_reverts(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) {
        match self.capture_call(executor, name, input) {
            evm::Capture::Exit((ExitReason::Revert(_), _)) => {}
            _ => panic!("function did not revert"),
        }
    }

    // Executor must be passed by value to get emitted events.
    pub fn events_emitted(&self, executor: Executor, events: &[(&str, &[ethabi::Token])]) {
        let raw_logs = executor
            .deconstruct()
            .1
            .into_iter()
            .map(|log| ethabi::RawLog::from((log.topics, log.data)))
            .collect::<Vec<ethabi::RawLog>>();

        for (name, expected_output) in events {
            let event = self
                .abi
                .events()
                .find(|event| event.name.eq(name))
                .expect("unable to find event for name");

            let outputs_for_event = raw_logs
                .iter()
                .filter_map(|raw_log| event.parse_log(raw_log.to_owned()).ok())
                .map(|event_log| {
                    event_log
                        .params
                        .into_iter()
                        .map(|param| param.value)
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();

            if !outputs_for_event.iter().any(|v| &v == expected_output) {
                panic!(
                    "no {} logs matching: {:?}\nfound: {:?}",
                    name, expected_output, outputs_for_event
                )
            }
        }
    }

    pub fn set_caller(&mut self, caller: H160) {
        self.caller = caller;
    }
}

#[allow(dead_code)]
pub fn with_executor(test: &dyn Fn(Executor)) {
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
    let backend = evm::backend::MemoryBackend::new(&vicinity, state);

    with_executor_backend(backend, test)
}

#[allow(dead_code)]
pub fn with_executor_backend(backend: evm::backend::MemoryBackend, test: &dyn Fn(Executor)) {
    let config = evm::Config::istanbul();
    let executor = evm::executor::StackExecutor::new(&backend, usize::max_value(), &config);

    test(executor)
}

#[allow(dead_code)]
pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let src = fs::read_to_string(format!("tests/fixtures/{}", fixture))
        .expect("unable to read fixture file");
    let compiled_module = compiler::compile(&src, true, false).expect("failed to compile module");
    let compiled_contract = compiled_module
        .contracts
        .get(contract_name)
        .expect("could not find contract in fixture");
    let abi = ethabi::Contract::load(StringReader::new(&compiled_contract.json_abi))
        .expect("unable to load the ABI");
    let mut bytecode = hex::decode(&compiled_contract.bytecode).expect("failed to decode bytecode");

    if let Some(constructor) = &abi.constructor {
        bytecode = constructor.encode_input(bytecode, init_params).unwrap()
    }

    if let evm::Capture::Exit(exit) = executor.create(
        address(DEFAULT_CALLER),
        evm_runtime::CreateScheme::Legacy {
            caller: address(DEFAULT_CALLER),
        },
        U256::zero(),
        bytecode,
        None,
    ) {
        return ContractHarness::new(exit.1.expect("Unable to retrieve contract address"), abi);
    }

    panic!("Failed to create contract")
}

#[allow(dead_code)]
pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    let src = fs::read_to_string(format!("tests/fixtures/{}", fixture))
        .expect("unable to read fixture file");
    let compiled_module = compiler::compile(&src, true, false).expect("failed to compile module");
    let compiled_contract = compiled_module
        .contracts
        .get(contract_name)
        .expect("could not find contract in fixture");
    let abi = ethabi::Contract::load(StringReader::new(&compiled_contract.json_abi))
        .expect("unable to load the ABI");

    ContractHarness::new(address, abi)
}

#[allow(dead_code)]
pub fn test_runtime_functions(
    executor: &mut Executor,
    functions: Vec<yul::Statement>,
    test_statements: Vec<yul::Statement>,
) {
    let all_statements = [functions, test_statements].concat();
    let yul_code = yul::Object {
        name: identifier! { Contract },
        code: code! { [all_statements...] },
        objects: vec![],
        data: vec![],
    }
    .to_string()
    .replace("\"", "\\\"");
    let bytecode = compiler::evm::compile_single_contract("Contract", yul_code, false)
        .expect("failed to compile Yul");
    let bytecode = hex::decode(&bytecode).expect("failed to decode bytecode");

    if let evm::Capture::Exit((reason, _, _)) = executor.create(
        address(DEFAULT_CALLER),
        evm_runtime::CreateScheme::Legacy {
            caller: address(DEFAULT_CALLER),
        },
        U256::zero(),
        bytecode,
        None,
    ) {
        if !matches!(reason, ExitReason::Succeed(_)) {
            panic!("Runtime function test failed: {:?}", reason)
        }
    } else {
        panic!("EVM trap during test")
    }
}

#[allow(dead_code)]
pub fn uint_token(n: usize) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

#[allow(dead_code)]
pub fn uint_token_from_dec_str(val: &str) -> ethabi::Token {
    ethabi::Token::Uint(U256::from_dec_str(val).expect("Not a valid dec string"))
}

#[allow(dead_code)]
pub fn int_token(val: isize) -> ethabi::Token {
    ethabi::Token::Int(to_2s_complement(val))
}

#[allow(dead_code)]
pub fn string_token(s: &str) -> ethabi::Token {
    ethabi::Token::String(s.to_string())
}

#[allow(dead_code)]
pub fn address(s: &str) -> H160 {
    H160::from_str(s).expect(&format!("couldn't create address from: {}", s))
}

#[allow(dead_code)]
pub fn address_token(s: &str) -> ethabi::Token {
    // left pads to 40 characters
    ethabi::Token::Address(address(&format!("{:0>40}", s)))
}

#[allow(dead_code)]
pub fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

#[allow(dead_code)]
pub fn bytes_token(s: &str) -> ethabi::Token {
    ethabi::Token::FixedBytes(ethabi::FixedBytes::from(s))
}

#[allow(dead_code)]
pub fn u256_array_token(v: &[usize]) -> ethabi::Token {
    ethabi::Token::FixedArray(v.iter().map(|n| uint_token(*n)).collect())
}

#[allow(dead_code)]
pub fn address_array_token(v: &[&str]) -> ethabi::Token {
    ethabi::Token::FixedArray(v.iter().map(|s| address_token(s)).collect())
}

#[allow(dead_code)]
pub fn to_2s_complement(val: isize) -> U256 {
    // Since this API takes an `isize` we can be sure that the min and max values
    // will never be above what fits the `I256` type which has the same capacity
    // as U256 but splits it so that one half covers numbers above 0 and the
    // other half covers the numbers below 0.

    // Conversion to Two's Complement: https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html

    if val >= 0 {
        U256::from(val)
    } else {
        let positive_val = val * -1;
        get_2s_complement_for_negative(U256::from(positive_val))
    }
}

/// To get the 2s complement value for e.g. -128 call
/// get_2s_complement_for_negative(128)
#[allow(dead_code)]
pub fn get_2s_complement_for_negative(assume_negative: U256) -> U256 {
    let (negated, _) = assume_negative.overflowing_neg();
    negated + 1
}

#[allow(dead_code)]
pub struct NumericAbiTokenBounds {
    pub size: usize,
    pub u_min: ethabi::Token,
    pub i_min: ethabi::Token,
    pub u_max: ethabi::Token,
    pub i_max: ethabi::Token,
}

impl NumericAbiTokenBounds {
    #[allow(dead_code)]
    pub fn get_all() -> [NumericAbiTokenBounds; 6] {
        let zero = uint_token(0);
        let u64_max = ethabi::Token::Uint(U256::from(2).pow(U256::from(64)) - 1);
        let i64_min = ethabi::Token::Int(get_2s_complement_for_negative(
            U256::from(2).pow(U256::from(63)),
        ));

        let u128_max = ethabi::Token::Uint(U256::from(2).pow(U256::from(128)) - 1);
        let i128_max = ethabi::Token::Int(U256::from(2).pow(U256::from(127)) - 1);
        let i128_min = ethabi::Token::Int(get_2s_complement_for_negative(
            U256::from(2).pow(U256::from(127)),
        ));

        let u256_max = ethabi::Token::Uint(U256::MAX);
        let i256_max = ethabi::Token::Int(U256::from(2).pow(U256::from(255)) - 1);
        let i256_min = ethabi::Token::Int(get_2s_complement_for_negative(
            U256::from(2).pow(U256::from(255)),
        ));

        let sizes = [
            NumericAbiTokenBounds {
                size: 8,
                u_min: zero.clone(),
                i_min: int_token(-128),
                u_max: uint_token(255),
                i_max: int_token(127),
            },
            NumericAbiTokenBounds {
                size: 16,
                u_min: zero.clone(),
                i_min: int_token(-32768),
                u_max: uint_token(65535),
                i_max: int_token(32767),
            },
            NumericAbiTokenBounds {
                size: 32,
                u_min: zero.clone(),
                i_min: int_token(-2147483648),
                u_max: uint_token(4294967295),
                i_max: int_token(2147483647),
            },
            NumericAbiTokenBounds {
                size: 64,
                u_min: zero.clone(),
                i_min: i64_min.clone(),
                u_max: u64_max.clone(),
                i_max: int_token(9223372036854775807),
            },
            NumericAbiTokenBounds {
                size: 128,
                u_min: zero.clone(),
                i_min: i128_min.clone(),
                u_max: u128_max.clone(),
                i_max: i128_max.clone(),
            },
            NumericAbiTokenBounds {
                size: 256,
                u_min: zero.clone(),
                i_min: i256_min.clone(),
                u_max: u256_max.clone(),
                i_max: i256_max.clone(),
            },
        ];

        sizes
    }
}
