#![cfg(feature = "solc-backend")]
use ethabi;
use evm;

use compiler::evm::CompileStage;
use evm_runtime::{
    ExitReason,
    Handler,
};
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

const DEFAULT_CALLER: &str = "1000000000000000000000000000000000000001";

struct ContractHarness {
    address: H160,
    abi: ethabi::Contract,
    caller: H160,
}

impl ContractHarness {
    fn new(contract_address: H160, abi: ethabi::Contract) -> Self {
        let caller = address(DEFAULT_CALLER);

        ContractHarness {
            address: contract_address,
            abi,
            caller,
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
            address: self.address.clone(),
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

        match self.capture_call(executor, name, &input) {
            evm::Capture::Exit((ExitReason::Succeed(_), actual_output)) => {
                if let Some(output) = output {
                    let actual_output = &function
                        .decode_output(&actual_output)
                        .expect(&format!("unable to decode output: {:?}", &actual_output))[0];

                    assert_eq!(&output, actual_output)
                }
            }
            evm::Capture::Exit((reason, _)) => panic!("failed to run \"{}\": {:?}", name, reason),
            _ => panic!("trap"),
        }
    }

    pub fn test_function_reverts(
        &self,
        executor: &mut Executor,
        name: &str,
        input: Vec<ethabi::Token>,
    ) {
        match self.capture_call(executor, name, &input) {
            evm::Capture::Exit((ExitReason::Revert(_), _)) => {}
            _ => panic!("function did not revert"),
        }
    }

    // Executor must be passed by value to get emitted events.
    pub fn events_emitted(&self, executor: Executor, events: Vec<(&str, Vec<ethabi::Token>)>) {
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

            if !outputs_for_event.contains(&expected_output) {
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

fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    name: &str,
    init_params: Vec<ethabi::Token>,
) -> ContractHarness {
    let src = fs::read_to_string(format!("tests/fixtures/{}", fixture))
        .expect("Unable to read fixture file");

    let output = compiler::evm::compile(&src, CompileStage::AllUpToBytecode)
        .expect("Unable to compile to bytecode");
    let json_abi = compiler::abi::build(&src)
        .expect("Unable to build the module ABIs")
        .contracts[name]
        .json(false)
        .expect("Unable to serialize the contract ABI.");

    let abi = ethabi::Contract::load(StringReader::new(&json_abi)).expect("Unable to load the ABI");
    let mut init_code = hex::decode(output.bytecode).unwrap();
    if let Some(constructor) = &abi.constructor {
        init_code = constructor.encode_input(init_code, &init_params).unwrap()
    }

    if let evm::Capture::Exit(exit) = executor.create(
        address(DEFAULT_CALLER),
        evm_runtime::CreateScheme::Legacy {
            caller: address(DEFAULT_CALLER),
        },
        U256::zero(),
        init_code,
        None,
    ) {
        return ContractHarness::new(exit.1.expect("Unable to retrieve contract address"), abi);
    }

    panic!("Failed to create contract")
}

fn uint_token(n: usize) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}

fn int_token(val: isize) -> ethabi::Token {
    ethabi::Token::Int(to_2s_complement(val))
}

fn string_token(s: &str) -> ethabi::Token {
    ethabi::Token::String(s.to_string())
}

fn address(s: &str) -> H160 {
    H160::from_str(s).expect("Couldn't create address from string")
}

fn address_token(s: &str) -> ethabi::Token {
    ethabi::Token::Address(address(s))
}

fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

fn bytes_token(s: &str) -> ethabi::Token {
    ethabi::Token::FixedBytes(ethabi::FixedBytes::from(s))
}

fn u256_array_token(v: Vec<usize>) -> ethabi::Token {
    ethabi::Token::FixedArray(v.into_iter().map(|n| uint_token(n)).collect())
}

fn to_2s_complement(val: isize) -> U256 {
    // Since this API takes an `isize` we can be sure that the min and max values
    // will never be above what fits the `I256` type which has the same capacity
    // as U256 but splits it so that one half covers numbers above 0 and the
    // other half covers the numbers below 0.

    // Conversion to Two's Complement: https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html

    if val >= 0 {
        return U256::from(val);
    } else {
        let positive_val = val * -1;
        let temp = U256::from(positive_val);
        let (negated, _) = temp.overflowing_neg();
        return negated + 1;
    }
}

#[test]
fn test_to_2s_complement() {
    let minus_three = U256::from_dec_str(
        "115792089237316195423570985008687907853269984665640564039457584007913129639933",
    )
    .unwrap();
    assert_eq!(minus_three, to_2s_complement(-3));
    assert_eq!(U256::from(3), to_2s_complement(3));
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

#[test]
fn test_revert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "revert.fe", "Foo", vec![]);

        let exit = harness.capture_call(&mut executor, "bar", &vec![]);

        assert!(matches!(
            exit,
            evm::Capture::Exit((evm::ExitReason::Revert(_), _))
        ))
    })
}

#[test]
fn test_assert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "assert.fe", "Foo", vec![]);

        let exit1 = harness.capture_call(&mut executor, "bar", &vec![uint_token(4)]);

        assert!(matches!(
            exit1,
            evm::Capture::Exit((evm::ExitReason::Revert(_), _))
        ));

        let exit2 = harness.capture_call(&mut executor, "bar", &vec![uint_token(42)]);

        assert!(matches!(
            exit2,
            evm::Capture::Exit((evm::ExitReason::Succeed(_), _))
        ))
    })
}

#[rstest(fixture_file, input, expected,
    case("while_loop_with_continue.fe", vec![], Some(uint_token(1))),
    case("while_loop.fe", vec![], Some(uint_token(3))),
    case("while_loop_with_break.fe", vec![], Some(uint_token(1))),
    case("while_loop_with_break_2.fe", vec![], Some(uint_token(1))),
    case("if_statement.fe", vec![uint_token(6)], Some(uint_token(1))),
    case("if_statement.fe", vec![uint_token(4)], Some(uint_token(0))),
    case("if_statement_2.fe", vec![uint_token(6)], Some(uint_token(1))),
    case("if_statement_with_block_declaration.fe", vec![], Some(uint_token(1))),
    case("ternary_expression.fe", vec![uint_token(6)], Some(uint_token(1))),
    case("ternary_expression.fe", vec![uint_token(4)], Some(uint_token(0))),
    case("call_statement_without_args.fe", vec![], Some(uint_token(100))),
    case("call_statement_with_args.fe", vec![], Some(uint_token(100))),
    case("call_statement_with_args_2.fe", vec![], Some(uint_token(100))),
    case("return_bool_true.fe", vec![], Some(bool_token(true))),
    case("return_bool_false.fe", vec![], Some(bool_token(false))),
    case("return_u256_from_called_fn_with_args.fe", vec![], Some(uint_token(200))),
    case("return_u256_from_called_fn.fe", vec![], Some(uint_token(42))),
    case("return_u256.fe", vec![], Some(uint_token(42))),
    case("return_i256.fe", vec![], Some(int_token(-3))),
    case("return_identity_u256.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_identity_u128.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_identity_u64.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_identity_u32.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_identity_u16.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_identity_u8.fe", vec![uint_token(42)], Some(uint_token(42))),
    case("return_u128_cast.fe", vec![], Some(uint_token(42))),
    case("return_i128_cast.fe", vec![], Some(int_token(-3))),
    // binary operators
    case("return_addition_u256.fe", vec![uint_token(42), uint_token(42)], Some(uint_token(84))),
    case("return_addition_i256.fe", vec![int_token(-42), int_token(-42)], Some(int_token(-84))),
    case("return_addition_i256.fe", vec![int_token(-42), int_token(42)], Some(int_token(0))),
    case("return_addition_u128.fe", vec![uint_token(42), uint_token(42)], Some(uint_token(84))),
    case("return_subtraction_u256.fe", vec![uint_token(42), uint_token(42)], Some(uint_token(0))),
    case("return_subtraction_i256.fe", vec![int_token(-42), int_token(-42)], Some(int_token(0))),
    case("return_subtraction_i256.fe", vec![int_token(-42), int_token(42)], Some(int_token(-84))),
    case("return_multiplication_u256.fe", vec![uint_token(42), uint_token(42)], Some(uint_token(1764))),
    case("return_multiplication_i256.fe", vec![int_token(-42), int_token(-42)], Some(int_token(1764))),
    case("return_multiplication_i256.fe", vec![int_token(-42), int_token(42)], Some(int_token(-1764))),
    case("return_division_u256.fe", vec![uint_token(42), uint_token(42)], Some(uint_token(1))),
    case("return_division_i256.fe", vec![int_token(-42), int_token(-42)], Some(int_token(1))),
    case("return_division_i256.fe", vec![int_token(-1), int_token(1)], Some(int_token(-1))),
    case("return_division_i256.fe", vec![int_token(-42), int_token(42)], Some(int_token(-1))),
    case("return_pow_u256.fe", vec![uint_token(2), uint_token(0)], Some(uint_token(1))),
    case("return_pow_u256.fe", vec![uint_token(2), uint_token(4)], Some(uint_token(16))),
    case("return_mod_u256.fe", vec![uint_token(5), uint_token(0)], Some(uint_token(0))),
    case("return_mod_u256.fe", vec![uint_token(5), uint_token(2)], Some(uint_token(1))),
    case("return_mod_u256.fe", vec![uint_token(5), uint_token(3)], Some(uint_token(2))),
    case("return_mod_u256.fe", vec![uint_token(5), uint_token(5)], Some(uint_token(0))),
    case("return_mod_i256.fe", vec![int_token(5), int_token(0)], Some(int_token(0))),
    case("return_mod_i256.fe", vec![int_token(5), int_token(2)], Some(int_token(1))),
    case("return_mod_i256.fe", vec![int_token(5), int_token(3)], Some(int_token(2))),
    case("return_mod_i256.fe", vec![int_token(5), int_token(5)], Some(int_token(0))),
    case("return_bitwiseand_u256.fe", vec![uint_token(12), uint_token(25)], Some(uint_token(8))),
    case("return_bitwiseand_u128.fe", vec![uint_token(12), uint_token(25)], Some(uint_token(8))),
    case("return_bitwiseor_u256.fe", vec![uint_token(12), uint_token(25)], Some(uint_token(29))),
    case("return_bitwisexor_u256.fe", vec![uint_token(12), uint_token(25)], Some(uint_token(21))),
    case("return_bitwiseshl_u256.fe", vec![uint_token(212), uint_token(0)], Some(uint_token(212))),
    case("return_bitwiseshl_u256.fe", vec![uint_token(212), uint_token(1)], Some(uint_token(424))),
    case("return_bitwiseshr_u256.fe", vec![uint_token(212), uint_token(0)], Some(uint_token(212))),
    case("return_bitwiseshr_u256.fe", vec![uint_token(212), uint_token(1)], Some(uint_token(106))),
    case("return_bitwiseshr_i256.fe", vec![int_token(212), int_token(0)], Some(int_token(212))),
    case("return_bitwiseshr_i256.fe", vec![int_token(212), int_token(1)], Some(int_token(106))),
    // comparision operators
    case("return_eq_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(true))),
    case("return_eq_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(false))),
    case("return_noteq_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(false))),
    case("return_noteq_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(true))),
    case("return_lt_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(true))),
    case("return_lt_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(false))),
    case("return_lt_u256.fe", vec![uint_token(2), uint_token(1)], Some(bool_token(false))),
    case("return_lt_u128.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(true))),
    // lt_i256 with positive and negative numbers
    case("return_lt_i256.fe", vec![int_token(1), int_token(2)], Some(bool_token(true))),
    case("return_lt_i256.fe", vec![int_token(1), int_token(1)], Some(bool_token(false))),
    case("return_lt_i256.fe", vec![int_token(2), int_token(1)], Some(bool_token(false))),
    case("return_lt_i256.fe", vec![int_token(-2), int_token(-1)], Some(bool_token(true))),
    case("return_lt_i256.fe", vec![int_token(-1), int_token(-1)], Some(bool_token(false))),
    case("return_lt_i256.fe", vec![int_token(-1), int_token(-2)], Some(bool_token(false))),
    case("return_lte_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(true))),
    case("return_lte_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(true))),
    // lte_i256 with positive and negative numbers
    case("return_lte_u256.fe", vec![uint_token(2), uint_token(1)], Some(bool_token(false))),
    case("return_lte_i256.fe", vec![int_token(1), int_token(2)], Some(bool_token(true))),
    case("return_lte_i256.fe", vec![int_token(1), int_token(1)], Some(bool_token(true))),
    case("return_lte_i256.fe", vec![int_token(2), int_token(1)], Some(bool_token(false))),
    case("return_lte_i256.fe", vec![int_token(-2), int_token(-1)], Some(bool_token(true))),
    case("return_lte_i256.fe", vec![int_token(-1), int_token(-1)], Some(bool_token(true))),
    case("return_lte_i256.fe", vec![int_token(-1), int_token(-2)], Some(bool_token(false))),
    case("return_gt_u256.fe", vec![uint_token(2), uint_token(1)], Some(bool_token(true))),
    case("return_gt_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(false))),
    case("return_gt_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(false))),
    // gt_i256 with positive and negative numbers
    case("return_gt_i256.fe", vec![int_token(2), int_token(1)], Some(bool_token(true))),
    case("return_gt_i256.fe", vec![int_token(1), int_token(1)], Some(bool_token(false))),
    case("return_gt_i256.fe", vec![int_token(1), int_token(2)], Some(bool_token(false))),
    case("return_gt_i256.fe", vec![int_token(-1), int_token(-2)], Some(bool_token(true))),
    case("return_gt_i256.fe", vec![int_token(-1), int_token(-1)], Some(bool_token(false))),
    case("return_gt_i256.fe", vec![int_token(-2), int_token(-1)], Some(bool_token(false))),
    case("return_gte_u256.fe", vec![uint_token(2), uint_token(1)], Some(bool_token(true))),
    case("return_gte_u256.fe", vec![uint_token(1), uint_token(1)], Some(bool_token(true))),
    case("return_gte_u256.fe", vec![uint_token(1), uint_token(2)], Some(bool_token(false))),
    // gte_i256 with positive and negative numbers
    case("return_gte_i256.fe", vec![int_token(2), int_token(1)], Some(bool_token(true))),
    case("return_gte_i256.fe", vec![int_token(1), int_token(1)], Some(bool_token(true))),
    case("return_gte_i256.fe", vec![int_token(1), int_token(2)], Some(bool_token(false))),
    case("return_gte_i256.fe", vec![int_token(-1), int_token(-2)], Some(bool_token(true))),
    case("return_gte_i256.fe", vec![int_token(-1), int_token(-1)], Some(bool_token(true))),
    case("return_gte_i256.fe", vec![int_token(-2), int_token(-1)], Some(bool_token(false))),
)]
fn test_method_return(
    fixture_file: &str,
    input: Vec<ethabi::Token>,
    expected: Option<ethabi::Token>,
) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo", vec![]);
        harness.test_function(&mut executor, "bar", input.clone(), expected.clone())
    })
}

#[test]
fn return_array() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_array.fe", "Foo", vec![]);

        harness.test_function(
            &mut executor,
            "bar",
            vec![uint_token(42)],
            Some(u256_array_token(vec![0, 0, 0, 42, 0])),
        )
    })
}

#[test]
fn multi_param() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "multi_param.fe", "Foo", vec![]);

        harness.test_function(
            &mut executor,
            "bar",
            vec![uint_token(4), uint_token(42), uint_token(420)],
            Some(u256_array_token(vec![4, 42, 420])),
        )
    })
}

#[rstest(
    fixture_file,
    case("u256_u256_map.fe"),
    case("u128_u128_map.fe"),
    case("u64_u64_map.fe"),
    case("u32_u32_map.fe"),
    case("u16_u16_map.fe"),
    case("u8_u8_map.fe")
)]
fn test_map(fixture_file: &str) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo", vec![]);

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![uint_token(4), uint_token(42)],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            vec![uint_token(420), uint_token(12)],
            None,
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            vec![uint_token(4)],
            Some(uint_token(42)),
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            vec![uint_token(420)],
            Some(uint_token(12)),
        );
    })
}

#[test]
fn address_bytes10_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "address_bytes10_map.fe", "Foo", vec![]);

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
        let mut harness = deploy_contract(&mut executor, "guest_book.fe", "GuestBook", vec![]);

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

        harness.events_emitted(executor, vec![("Signed", vec![bytes])]);
    })
}

#[test]
fn return_sender() {
    with_executor(&|mut executor| {
        let mut harness = deploy_contract(&mut executor, "return_sender.fe", "Foo", vec![]);

        let sender = address_token("1234000000000000000000000000000000005678");

        harness.caller = sender.clone().to_address().unwrap();

        harness.test_function(&mut executor, "bar", vec![], Some(sender));
    })
}

#[test]
fn nested_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "nested_map.fe", "Foo", vec![]);

        let address1 = address_token("1000000000000000000000000000000000000001");
        let address2 = address_token("2000000000000000000000000000000000000002");
        let address3 = address_token("3000000000000000000000000000000000000003");

        // write bar (address -> address -> u256)
        harness.test_function(
            &mut executor,
            "write_bar",
            vec![address1.clone(), address2.clone(), uint_token(12)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            vec![address1.clone(), address3.clone(), uint_token(13)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            vec![address2.clone(), address1.clone(), uint_token(21)],
            None,
        );

        // write baz (address -> u256 -> bool)
        harness.test_function(
            &mut executor,
            "write_baz",
            vec![address1.clone(), uint_token(26), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            vec![address2.clone(), uint_token(42), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            vec![address2.clone(), uint_token(100), bool_token(false)],
            None,
        );

        // read bar
        harness.test_function(
            &mut executor,
            "read_bar",
            vec![address1.clone(), address2.clone()],
            Some(uint_token(12)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            vec![address1.clone(), address3.clone()],
            Some(uint_token(13)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            vec![address2.clone(), address1.clone()],
            Some(uint_token(21)),
        );

        // read baz
        harness.test_function(
            &mut executor,
            "read_baz",
            vec![address1.clone(), uint_token(26)],
            Some(bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            vec![address2.clone(), uint_token(42)],
            Some(bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            vec![address2.clone(), uint_token(100)],
            Some(bool_token(false)),
        );
    })
}

#[test]
fn events() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "events.fe", "Foo", vec![]);

        let addr1 = address_token("1234000000000000000000000000000000005678");
        let addr2 = address_token("9123000000000000000000000000000000004567");
        let addr_array = ethabi::Token::FixedArray(vec![addr1.clone(), addr2.clone()]);
        let bytes = bytes_token(
            iter::repeat("ten bytes.")
                .take(10)
                .collect::<String>()
                .as_str(),
        );

        harness.test_function(&mut executor, "emit_nums", vec![], None);
        harness.test_function(&mut executor, "emit_bases", vec![addr1.clone()], None);
        harness.test_function(
            &mut executor,
            "emit_mix",
            vec![addr1.clone(), bytes.clone()],
            None,
        );
        harness.test_function(
            &mut executor,
            "emit_addresses",
            vec![addr1.clone(), addr2.clone()],
            None,
        );

        harness.events_emitted(
            executor,
            vec![
                ("Nums", vec![uint_token(26), uint_token(42)]),
                ("Bases", vec![uint_token(26), addr1.clone()]),
                (
                    "Mix",
                    vec![uint_token(26), addr1.clone(), uint_token(42), bytes],
                ),
                ("Addresses", vec![addr_array]),
            ],
        );
    })
}

#[test]
fn constructor() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "constructor.fe",
            "Foo",
            vec![uint_token(26), uint_token(42)],
        );

        harness.test_function(&mut executor, "read_bar", vec![], Some(uint_token(68)));
    })
}

#[test]
fn strings() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "strings.fe",
            "Foo",
            vec![
                string_token("string 1"),
                address_token("1000000000000000000000000000000000000001"),
                string_token("string 2"),
                uint_token(42),
                string_token("string 3"),
            ],
        );

        harness.test_function(
            &mut executor,
            "bar",
            vec![string_token("string 4"), string_token("string 5")],
            Some(string_token("string 5")),
        );

        harness.events_emitted(
            executor,
            vec![(
                "MyEvent",
                vec![
                    string_token("string 2"),
                    uint_token(42),
                    string_token("string 1"),
                    string_token("string 3"),
                    address_token("1000000000000000000000000000000000000001"),
                ],
            )],
        );
    });
}

#[test]
fn sized_vals_in_sto() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "sized_vals_in_sto.fe", "Foo", vec![]);

        let num = uint_token(68);
        let nums = u256_array_token((0..42).into_iter().collect());
        let string = string_token("there are 26 protons in fe");

        harness.test_function(&mut executor, "write_num", vec![num.clone()], None);
        harness.test_function(&mut executor, "read_num", vec![], Some(num.clone()));

        harness.test_function(&mut executor, "write_nums", vec![nums.clone()], None);
        harness.test_function(&mut executor, "read_nums", vec![], Some(nums.clone()));

        harness.test_function(&mut executor, "write_str", vec![string.clone()], None);
        harness.test_function(&mut executor, "read_str", vec![], Some(string.clone()));

        harness.test_function(&mut executor, "emit_event", vec![], None);
        harness.events_emitted(executor, vec![("MyEvent", vec![num, nums, string])]);
    });
}

#[test]
fn erc20_token() {
    with_executor(&|mut executor| {
        let token_name = string_token("Fe Coin");
        let token_symbol = string_token("fe");

        let mut harness = deploy_contract(
            &mut executor,
            "erc20_token.fe",
            "ERC20",
            vec![token_name.clone(), token_symbol.clone()],
        );

        let alice = DEFAULT_CALLER;
        let bob = "2000000000000000000000000000000000000002";
        let james = "3000000000000000000000000000000000000003";

        // validate state after init
        // alice starts with 2600 Fe Coins
        harness.test_function(&mut executor, "name", vec![], Some(token_name));
        harness.test_function(&mut executor, "symbol", vec![], Some(token_symbol));
        harness.test_function(&mut executor, "decimals", vec![], Some(uint_token(18)));
        harness.test_function(&mut executor, "totalSupply", vec![], Some(uint_token(2600)));
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(alice)],
            Some(uint_token(2600)),
        );

        // transfer from alice to bob
        harness.test_function(
            &mut executor,
            "transfer",
            vec![address_token(bob), uint_token(42)],
            Some(bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(bob)],
            Some(uint_token(42)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(alice)],
            Some(uint_token(2558)),
        );

        // approve and transfer
        // alice approves bob to send 50 Fe Coins
        harness.test_function(
            &mut executor,
            "approve",
            vec![address_token(bob), uint_token(50)],
            Some(bool_token(true)),
        );
        harness.set_caller(address(bob));
        harness.test_function(
            &mut executor,
            "transferFrom",
            vec![address_token(alice), address_token(james), uint_token(25)],
            Some(bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(alice)],
            Some(uint_token(2533)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(james)],
            Some(uint_token(25)),
        );
        harness.test_function(
            &mut executor,
            "allowance",
            vec![address_token(alice), address_token(bob)],
            Some(uint_token(25)),
        );
        harness.test_function_reverts(
            &mut executor,
            "transferFrom",
            vec![address_token(alice), address_token(bob), uint_token(50)],
        );
        harness.test_function(
            &mut executor,
            "transferFrom",
            vec![address_token(alice), address_token(james), uint_token(20)],
            Some(bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "balanceOf",
            vec![address_token(james)],
            Some(uint_token(45)),
        );

        // validate events
        harness.events_emitted(
            executor,
            vec![
                (
                    "Transfer",
                    vec![address_token(alice), address_token(bob), uint_token(42)],
                ),
                (
                    "Transfer",
                    vec![address_token(alice), address_token(james), uint_token(25)],
                ),
                (
                    "Transfer",
                    vec![address_token(alice), address_token(james), uint_token(20)],
                ),
                (
                    "Approval",
                    vec![address_token(alice), address_token(bob), uint_token(50)],
                ),
                (
                    "Approval",
                    vec![address_token(alice), address_token(bob), uint_token(25)],
                ),
                (
                    "Approval",
                    vec![address_token(alice), address_token(bob), uint_token(5)],
                ),
            ],
        );
    });
}
