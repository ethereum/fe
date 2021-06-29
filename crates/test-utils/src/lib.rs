#![cfg(feature = "solc-backend")]
use compiler::errors::{AnalyzerError, CompileError, ErrorKind};
use evm_runtime::{ExitReason, Handler};
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::{FileStore, SourceFileId};
use fe_compiler as compiler;
use fe_compiler::yul::runtime::functions;
use primitive_types::{H160, H256, U256};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::str::FromStr;
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
pub type Backend<'a> = evm::backend::MemoryBackend<'a>;

#[allow(dead_code)]
pub type StackState<'a> = evm::executor::MemoryStackState<'a, 'a, Backend<'a>>;

#[allow(dead_code)]
pub type Executor<'a> = evm::executor::StackExecutor<'a, StackState<'a>>;

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
            .unwrap_or_else(|_| panic!("Unable to encode input for {}", name));

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
            evm::Capture::Exit((ExitReason::Succeed(_), output)) => function
                .decode_output(&output)
                .unwrap_or_else(|_| panic!("unable to decode output of {}: {:?}", name, &output))
                .pop(),
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
            .into_state()
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

            if !outputs_for_event.iter().any(|v| v == expected_output) {
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
pub fn with_executor_backend(backend: Backend, test: &dyn Fn(Executor)) {
    let config = evm::Config::istanbul();
    let stack_state = StackState::new(
        evm::executor::StackSubstateMetadata::new(u64::MAX, &config),
        &backend,
    );
    let executor = Executor::new(stack_state, &config);

    test(executor)
}

pub fn read_fixture(path: &str) -> (String, SourceFileId) {
    let file_path = Path::new(path);
    let absolute_path = fs::canonicalize(file_path)
        .unwrap_or_else(|_| panic!("unable to find the file at: {:?}", file_path));
    let mut files = FileStore::new();
    files
        .load_file(absolute_path.to_str().unwrap())
        .unwrap_or_else(|_| panic!("unable to read fixture file: {}", path))
}

fn print_compiler_errors(error: CompileError, files: &FileStore) {
    for err in error.errors {
        match err {
            ErrorKind::Str(err) => eprintln!("Compiler error: {}", err),
            ErrorKind::Analyzer(AnalyzerError(diagnostics)) => {
                print_diagnostics(&diagnostics, &files);
            }
            ErrorKind::Parser(diags) => print_diagnostics(&diags, &files),
        }
    }
}

#[allow(dead_code)]
pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let mut files = FileStore::new();
    let (src, id) = files
        .load_file(&fixture)
        .expect("unable to read fixture file");

    let compiled_module = match compiler::compile(&src, id, true, true) {
        Ok(module) => module,
        Err(error) => {
            print_compiler_errors(error, &files);
            panic!("failed to compile module: {}", fixture)
        }
    };

    let compiled_contract = compiled_module
        .contracts
        .get(contract_name)
        .expect("could not find contract in fixture");

    _deploy_contract(
        executor,
        &compiled_contract.bytecode,
        &compiled_contract.json_abi,
        init_params,
    )
}

#[allow(dead_code)]
pub fn deploy_solidity_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let src = fs::read_to_string(&fixture)
        .expect("unable to read fixture file")
        .replace("\n", "")
        .replace("\"", "\\\"");

    let (bytecode, abi) =
        compile_solidity_contract(contract_name, &src).expect("Could not compile contract");

    _deploy_contract(executor, &bytecode, &abi, init_params)
}

#[allow(dead_code)]
pub fn encode_error_reason(reason: &str) -> Vec<u8> {
    // Function selector for Error(string)
    const SELECTOR: &str = "08c379a0";
    // Data offset
    const DATA_OFFSET: &str = "0000000000000000000000000000000000000000000000000000000000000020";

    // Length of the string padded to 32 bit hex
    let string_len = format!("{:0>64x}", reason.len());

    let mut string_bytes = reason.as_bytes().to_vec();
    while string_bytes.len() % 32 != 0 {
        string_bytes.push(0)
    }
    // The bytes of the string itself, right padded to consume a multiple of 32
    // bytes
    let string_bytes = hex::encode(&string_bytes);

    let all = format!("{}{}{}{}", SELECTOR, DATA_OFFSET, string_len, string_bytes);
    hex::decode(&all).unwrap_or_else(|_| panic!("No valid hex: {}", &all))
}

fn _deploy_contract(
    executor: &mut Executor,
    bytecode: &str,
    abi: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let abi = ethabi::Contract::load(abi.as_bytes()).expect("unable to load the ABI");

    let mut bytecode = hex::decode(bytecode).expect("failed to decode bytecode");

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

#[derive(Debug)]
pub struct SolidityCompileError(Vec<serde_json::Value>);

impl std::fmt::Display for SolidityCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.0[..])
    }
}

impl std::error::Error for SolidityCompileError {}

pub fn compile_solidity_contract(
    name: &str,
    solidity_src: &str,
) -> Result<(String, String), SolidityCompileError> {
    let solc_config = r#"
    {
        "language": "Solidity",
        "sources": { "input.sol": { "content": "{src}" } },
        "settings": {
          "outputSelection": { "*": { "*": ["*"], "": [ "*" ] } }
        }
      }
    "#;
    let solc_config = solc_config.replace("{src}", &solidity_src);

    let raw_output = solc::compile(&solc_config);

    let output: serde_json::Value =
        serde_json::from_str(&raw_output).expect("Unable to compile contract");

    if output["errors"].is_array() {
        let severity: serde_json::Value =
            serde_json::to_value("error").expect("Unable to convert into serde value type");
        let errors: serde_json::Value = output["errors"]
            .as_array()
            .unwrap()
            .iter()
            .cloned()
            .filter_map(|err| {
                if err["severity"] == severity {
                    Some(err["formattedMessage"].clone())
                } else {
                    None
                }
            })
            .collect();

        let errors_list = errors
            .as_array()
            .unwrap_or_else(|| panic!("Unable to parse error properly"));
        if !errors_list.is_empty() {
            return Err(SolidityCompileError(errors_list.clone()));
        }
    }

    let bytecode = output["contracts"]["input.sol"][name]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    let abi = output["contracts"]["input.sol"][name]["abi"].to_string();

    if [&bytecode, &abi].iter().any(|val| val == &"null") {
        return Err(SolidityCompileError(vec![serde_json::Value::String(
            String::from("Bytecode not found"),
        )]));
    }

    Ok((bytecode, abi))
}

#[allow(dead_code)]
pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    let (src, id) = read_fixture(&fixture);
    let compiled_module =
        compiler::compile(&src, id, true, true).expect("failed to compile module");
    let compiled_contract = compiled_module
        .contracts
        .get(contract_name)
        .expect("could not find contract in fixture");
    let abi = ethabi::Contract::load(compiled_contract.json_abi.as_bytes())
        .expect("unable to load the ABI");

    ContractHarness::new(address, abi)
}
pub struct Runtime {
    functions: Vec<yul::Statement>,
    test_statements: Vec<yul::Statement>,
    data: Vec<yul::Data>,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new().with_functions(functions::std())
    }
}

pub struct ExecutionOutput {
    exit_reason: ExitReason,
    data: Vec<u8>,
}

#[allow(dead_code)]
impl Runtime {
    /// Create a new `Runtime` instance.
    pub fn new() -> Runtime {
        Runtime {
            functions: vec![],
            test_statements: vec![],
            data: vec![],
        }
    }

    /// Add the given set of functions
    pub fn with_functions(self, fns: Vec<yul::Statement>) -> Runtime {
        Runtime {
            functions: fns,
            ..self
        }
    }

    /// Add the given set of test statements
    pub fn with_test_statements(self, statements: Vec<yul::Statement>) -> Runtime {
        Runtime {
            test_statements: statements,
            ..self
        }
    }

    // Add the given set of data
    pub fn with_data(self, data: Vec<yul::Data>) -> Runtime {
        Runtime { data, ..self }
    }

    /// Generate the top level YUL object
    pub fn to_yul(&self) -> yul::Object {
        let all_statements = [self.functions.clone(), self.test_statements.clone()].concat();
        yul::Object {
            name: identifier! { Contract },
            code: code! { [all_statements...] },
            objects: vec![],
            data: self.data.clone(),
        }
    }

    pub fn execute(&self, executor: &mut Executor) -> ExecutionOutput {
        let (exit_reason, data) = execute_runtime_functions(executor, &self);
        ExecutionOutput::new(exit_reason, data)
    }
}

#[allow(dead_code)]
impl ExecutionOutput {
    /// Create an `ExecutionOutput` instance
    pub fn new(exit_reason: ExitReason, data: Vec<u8>) -> ExecutionOutput {
        ExecutionOutput { exit_reason, data }
    }

    /// Panic if the execution did not succeed.
    pub fn expect_success(self) -> ExecutionOutput {
        if let ExecutionOutput {
            exit_reason: ExitReason::Succeed(_),
            ..
        } = &self
        {
            self
        } else {
            panic!("Execution did not succeed: {:?}", &self.exit_reason)
        }
    }

    /// Panic if the execution did not revert.
    pub fn expect_revert(self) -> ExecutionOutput {
        if let ExecutionOutput {
            exit_reason: ExitReason::Revert(_),
            ..
        } = &self
        {
            self
        } else {
            panic!("Execution did not revert: {:?}", &self.exit_reason)
        }
    }

    /// Panic if the output is not an encoded error reason of the given string.
    pub fn expect_revert_reason(self, reason: &str) -> ExecutionOutput {
        assert_eq!(self.data, encode_error_reason(reason));
        self
    }
}

fn execute_runtime_functions(executor: &mut Executor, runtime: &Runtime) -> (ExitReason, Vec<u8>) {
    let yul_code = runtime.to_yul().to_string().replace("\"", "\\\"");
    let bytecode = compiler::evm::compile_single_contract("Contract", yul_code, false)
        .expect("failed to compile Yul");
    let bytecode = hex::decode(&bytecode).expect("failed to decode bytecode");

    if let evm::Capture::Exit((reason, _, output)) = executor.create(
        address(DEFAULT_CALLER),
        evm_runtime::CreateScheme::Legacy {
            caller: address(DEFAULT_CALLER),
        },
        U256::zero(),
        bytecode,
        None,
    ) {
        (reason, output)
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
    H160::from_str(s).unwrap_or_else(|_| panic!("couldn't create address from: {}", s))
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
pub fn bytes32(val: &str) -> Vec<u8> {
    H256::from_str(val)
        .unwrap_or_else(|_| panic!("couldn't create bytes[32] from: {}", val))
        .as_bytes()
        .to_vec()
}

#[allow(dead_code)]
pub fn bytes32_token(val: &str) -> ethabi::Token {
    ethabi::Token::FixedBytes(bytes32(&format! {"{:0<64}", val}))
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
pub fn tuple_token(tokens: &[ethabi::Token]) -> ethabi::Token {
    ethabi::Token::Tuple(tokens.to_owned())
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
        let positive_val = -val;
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

        [
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
                i_min: i64_min,
                u_max: u64_max,
                i_max: int_token(9223372036854775807),
            },
            NumericAbiTokenBounds {
                size: 128,
                u_min: zero.clone(),
                i_min: i128_min,
                u_max: u128_max,
                i_max: i128_max,
            },
            NumericAbiTokenBounds {
                size: 256,
                u_min: zero,
                i_min: i256_min,
                u_max: u256_max,
                i_max: i256_max,
            },
        ]
    }
}
