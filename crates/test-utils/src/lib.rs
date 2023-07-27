use evm_runtime::{ExitReason, Handler};
use fe_common::diagnostics::print_diagnostics;
use fe_common::utils::keccak;
use fe_driver as driver;
use primitive_types::{H160, U256};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use yultsur::*;

#[macro_export]
macro_rules! assert_harness_gas_report {
    ($harness: expr) => {
        assert_snapshot!(format!("{}", $harness.gas_reporter));
    };

    ($harness: expr, $($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        let suffix = format!("{:?}", $($expr,)*).replace("\"", "");
        settings.set_snapshot_suffix(suffix);
        let _guard = settings.bind_to_scope();
        assert_snapshot!(format!("{}", $harness.gas_reporter));
    }
}

#[derive(Default, Debug)]
pub struct GasReporter {
    records: RefCell<Vec<GasRecord>>,
}

impl GasReporter {
    pub fn add_record(&self, description: &str, gas_used: u64) {
        self.records.borrow_mut().push(GasRecord {
            description: description.to_string(),
            gas_used,
        })
    }

    pub fn add_func_call_record(&self, function: &str, input: &[ethabi::Token], gas_used: u64) {
        let description = format!("{function}({input:?})");
        self.add_record(&description, gas_used)
    }
}

impl Display for GasReporter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for record in self.records.borrow().iter() {
            writeln!(f, "{} used {} gas", record.description, record.gas_used)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct GasRecord {
    pub description: String,
    pub gas_used: u64,
}

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
pub type StackState<'a> = evm::executor::stack::MemoryStackState<'a, 'a, Backend<'a>>;

#[allow(dead_code)]
pub type Executor<'a, 'b> = evm::executor::stack::StackExecutor<'a, 'b, StackState<'a>, ()>;

#[allow(dead_code)]
pub const DEFAULT_CALLER: &str = "1000000000000000000000000000000000000001";

#[allow(dead_code)]
pub struct ContractHarness {
    pub gas_reporter: GasReporter,
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
            gas_reporter: GasReporter::default(),
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
        let input = self.build_calldata(name, input);
        self.capture_call_raw_bytes(executor, input)
    }

    pub fn build_calldata(&self, name: &str, input: &[ethabi::Token]) -> Vec<u8> {
        let function = &self.abi.functions[name][0];
        function
            .encode_input(input)
            .unwrap_or_else(|reason| panic!("Unable to encode input for {name}: {reason:?}"))
    }

    pub fn capture_call_raw_bytes(
        &self,
        executor: &mut Executor,
        input: Vec<u8>,
    ) -> evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible> {
        let context = evm::Context {
            address: self.address,
            caller: self.caller,
            apparent_value: self.value,
        };

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

        assert_eq!(
            output.map(ToOwned::to_owned),
            actual_output,
            "unexpected output from `fn {name}`"
        )
    }

    pub fn call_function(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
    ) -> Option<ethabi::Token> {
        let function = &self.abi.functions[name][0];
        let start_gas = executor.used_gas();
        let capture = self.capture_call(executor, name, input);
        let gas_used = executor.used_gas() - start_gas;
        self.gas_reporter
            .add_func_call_record(name, input, gas_used);

        match capture {
            evm::Capture::Exit((ExitReason::Succeed(_), output)) => function
                .decode_output(&output)
                .unwrap_or_else(|_| panic!("unable to decode output of {}: {:?}", name, &output))
                .pop(),
            evm::Capture::Exit((reason, _)) => panic!("failed to run \"{name}\": {reason:?}"),
            evm::Capture::Trap(_) => panic!("trap"),
        }
    }

    pub fn test_function_reverts(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
        revert_data: &[u8],
    ) {
        validate_revert(self.capture_call(executor, name, input), revert_data)
    }

    pub fn test_call_reverts(&self, executor: &mut Executor, input: Vec<u8>, revert_data: &[u8]) {
        validate_revert(self.capture_call_raw_bytes(executor, input), revert_data)
    }

    pub fn test_function_returns(
        &self,
        executor: &mut Executor,
        name: &str,
        input: &[ethabi::Token],
        return_data: &[u8],
    ) {
        validate_return(self.capture_call(executor, name, input), return_data)
    }

    pub fn test_call_returns(&self, executor: &mut Executor, input: Vec<u8>, return_data: &[u8]) {
        validate_return(self.capture_call_raw_bytes(executor, input), return_data)
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
                .filter_map(|raw_log| event.parse_log(raw_log.clone()).ok())
                .map(|event_log| {
                    event_log
                        .params
                        .into_iter()
                        .map(|param| param.value)
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();

            if !outputs_for_event.iter().any(|v| v == expected_output) {
                println!("raw logs dump: {raw_logs:?}");
                panic!(
                    "no \"{name}\" logs matching: {expected_output:?}\nfound: {outputs_for_event:?}"
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
        block_base_fee_per_gas: U256::zero(),
    };
    let state: BTreeMap<primitive_types::H160, evm::backend::MemoryAccount> = BTreeMap::new();
    let backend = evm::backend::MemoryBackend::new(&vicinity, state);

    with_executor_backend(backend, test)
}

#[allow(dead_code)]
pub fn with_executor_backend(backend: Backend, test: &dyn Fn(Executor)) {
    let config = evm::Config::london();
    let stack_state = StackState::new(
        evm::executor::stack::StackSubstateMetadata::new(u64::MAX, &config),
        &backend,
    );
    let executor = Executor::new_with_precompiles(stack_state, &config, &());

    test(executor)
}

pub fn validate_revert(
    capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    expected_data: &[u8],
) {
    if let evm::Capture::Exit((evm::ExitReason::Revert(_), output)) = capture {
        assert_eq!(
            format!("0x{}", hex::encode(output)),
            format!("0x{}", hex::encode(expected_data))
        );
    } else {
        panic!("Method was expected to revert but didn't")
    };
}

pub fn validate_return(
    capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    expected_data: &[u8],
) {
    if let evm::Capture::Exit((evm::ExitReason::Succeed(_), output)) = capture {
        assert_eq!(
            format!("0x{}", hex::encode(output)),
            format!("0x{}", hex::encode(expected_data))
        );
    } else {
        panic!("Method was expected to return but didn't")
    };
}

pub fn encoded_panic_assert() -> Vec<u8> {
    encode_revert("Panic(uint256)", &[uint_token(0x01)])
}

pub fn encoded_over_or_underflow() -> Vec<u8> {
    encode_revert("Panic(uint256)", &[uint_token(0x11)])
}

pub fn encoded_panic_out_of_bounds() -> Vec<u8> {
    encode_revert("Panic(uint256)", &[uint_token(0x32)])
}

pub fn encoded_div_or_mod_by_zero() -> Vec<u8> {
    encode_revert("Panic(uint256)", &[uint_token(0x12)])
}

pub fn encoded_invalid_abi_data() -> Vec<u8> {
    encode_revert("Error(uint256)", &[uint_token(0x103)])
}

#[allow(dead_code)]
#[cfg(feature = "solc-backend")]
pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let mut db = driver::Db::default();
    let compiled_module = match driver::compile_single_file(
        &mut db,
        fixture,
        test_files::fixture(fixture),
        true,
        true,
    ) {
        Ok(module) => module,
        Err(error) => {
            fe_common::diagnostics::print_diagnostics(&db, &error.0);
            panic!("failed to compile module: {fixture}")
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
#[cfg(feature = "solc-backend")]
pub fn deploy_contract_from_ingot(
    executor: &mut Executor,
    path: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    use fe_common::utils::files::BuildFiles;

    let files = test_files::fixture_dir_files("ingots");
    let build_files = BuildFiles::load_static(files, path).expect("failed to load build files");
    let mut db = driver::Db::default();
    let compiled_module = match driver::compile_ingot(&mut db, &build_files, true, true) {
        Ok(module) => module,
        Err(error) => {
            fe_common::diagnostics::print_diagnostics(&db, &error.0);
            panic!("failed to compile ingot: {path}")
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
#[cfg(feature = "solc-backend")]
pub fn deploy_solidity_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
    optimized: bool,
) -> ContractHarness {
    let src = test_files::fixture(fixture)
        .replace('\n', "")
        .replace('"', "\\\"");

    let (bytecode, abi) = compile_solidity_contract(contract_name, &src, optimized)
        .expect("Could not compile contract");

    _deploy_contract(executor, &bytecode, &abi, init_params)
}

#[allow(dead_code)]
pub fn encode_error_reason(reason: &str) -> Vec<u8> {
    encode_revert("Error(string)", &[string_token(reason)])
}

#[allow(dead_code)]
pub fn encode_revert(selector: &str, input: &[ethabi::Token]) -> Vec<u8> {
    let mut data = String::new();
    for param in input {
        let encoded = match param {
            ethabi::Token::Uint(val) | ethabi::Token::Int(val) => {
                format!("{:0>64}", format!("{val:x}"))
            }
            ethabi::Token::Bool(val) => format!("{:0>64x}", *val as i32),
            ethabi::Token::String(val) => {
                const DATA_OFFSET: &str =
                    "0000000000000000000000000000000000000000000000000000000000000020";

                // Length of the string padded to 32 bit hex
                let string_len = format!("{:0>64x}", val.len());

                let mut string_bytes = val.as_bytes().to_vec();
                while string_bytes.len() % 32 != 0 {
                    string_bytes.push(0)
                }
                // The bytes of the string itself, right padded to consume a multiple of 32
                // bytes
                let string_bytes = hex::encode(&string_bytes);

                format!("{DATA_OFFSET}{string_len}{string_bytes}")
            }
            _ => todo!("Other ABI types not supported yet"),
        };
        data.push_str(&encoded);
    }

    let all = format!("{}{}", get_function_selector(selector), data);
    hex::decode(&all).unwrap_or_else(|_| panic!("No valid hex: {}", &all))
}

fn get_function_selector(signature: &str) -> String {
    // Function selector (e.g first 4 bytes of keccak("Error(string)")
    hex::encode(&keccak::full_as_bytes(signature.as_bytes())[..4])
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
        return ContractHarness::new(
            exit.1
                .unwrap_or_else(|| panic!("Unable to retrieve contract address: {:?}", exit.0)),
            abi,
        );
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

#[cfg(feature = "solc-backend")]
pub fn compile_solidity_contract(
    name: &str,
    solidity_src: &str,
    optimized: bool,
) -> Result<(String, String), SolidityCompileError> {
    let solc_config = r#"
    {
        "language": "Solidity",
        "sources": { "input.sol": { "content": "{src}" } },
        "settings": {
          "optimizer": { "enabled": {optimizer_enabled} },
          "outputSelection": { "*": { "*": ["*"], "": [ "*" ] } }
        }
      }
    "#;
    let solc_config = solc_config
        .replace("{src}", solidity_src)
        .replace("{optimizer_enabled}", &optimized.to_string());

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
        .replace('"', "");

    let abi = if let serde_json::Value::Array(data) = &output["contracts"]["input.sol"][name]["abi"]
    {
        data.iter()
            .cloned()
            .filter(|val| {
                // ethabi doesn't yet support error types so we just filter them out for now
                // https://github.com/rust-ethereum/ethabi/issues/225
                val["type"] != "error"
            })
            .collect::<Vec<_>>()
    } else {
        vec![]
    };

    let abi = serde_json::Value::Array(abi).to_string();

    if [&bytecode, &abi].iter().any(|val| val == &"null") {
        return Err(SolidityCompileError(vec![serde_json::Value::String(
            String::from("Bytecode not found"),
        )]));
    }

    Ok((bytecode, abi))
}

#[allow(dead_code)]
pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    let mut db = driver::Db::default();
    let compiled_module =
        driver::compile_single_file(&mut db, fixture, test_files::fixture(fixture), true, true)
            .unwrap_or_else(|err| {
                print_diagnostics(&db, &err.0);
                panic!("failed to compile fixture: {fixture}");
            });
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
        Self::new()
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

    #[cfg(feature = "solc-backend")]
    pub fn execute(&self, executor: &mut Executor) -> ExecutionOutput {
        let (exit_reason, data) = execute_runtime_functions(executor, self);
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

#[cfg(feature = "solc-backend")]
fn execute_runtime_functions(executor: &mut Executor, runtime: &Runtime) -> (ExitReason, Vec<u8>) {
    let yul_code = runtime.to_yul().to_string().replace('"', "\\\"");
    let bytecode = fe_yulc::compile_single_contract("Contract", &yul_code, false)
        .expect("failed to compile Yul");
    let bytecode = hex::decode(bytecode).expect("failed to decode bytecode");

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
pub fn uint_token(n: u64) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

#[allow(dead_code)]
pub fn uint_token_from_dec_str(val: &str) -> ethabi::Token {
    ethabi::Token::Uint(U256::from_dec_str(val).expect("Not a valid dec string"))
}

#[allow(dead_code)]
pub fn int_token(val: i64) -> ethabi::Token {
    ethabi::Token::Int(to_2s_complement(val))
}

#[allow(dead_code)]
pub fn string_token(s: &str) -> ethabi::Token {
    ethabi::Token::String(s.to_string())
}

#[allow(dead_code)]
pub fn address(s: &str) -> H160 {
    H160::from_str(s).unwrap_or_else(|_| panic!("couldn't create address from: {s}"))
}

#[allow(dead_code)]
pub fn address_token(s: &str) -> ethabi::Token {
    // left pads to 40 characters
    ethabi::Token::Address(address(&format!("{s:0>40}")))
}

#[allow(dead_code)]
pub fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

#[allow(dead_code)]
pub fn bytes_token(s: &str) -> ethabi::Token {
    ethabi::Token::Bytes(ethabi::Bytes::from(s))
}

#[allow(dead_code)]
pub fn uint_array_token(v: &[u64]) -> ethabi::Token {
    ethabi::Token::FixedArray(v.iter().map(|n| uint_token(*n)).collect())
}

#[allow(dead_code)]
pub fn int_array_token(v: &[i64]) -> ethabi::Token {
    ethabi::Token::FixedArray(v.iter().map(|n| int_token(*n)).collect())
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
pub fn to_2s_complement(val: i64) -> U256 {
    // Since this API takes an `i64` we can be sure that the min and max values
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
    assume_negative.overflowing_neg().0
}

#[allow(dead_code)]
pub struct NumericAbiTokenBounds {
    pub size: u64,
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
