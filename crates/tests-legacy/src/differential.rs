//! Tests that check for differences between Solidity and Fe implementations of similar contracts
#![cfg(all(feature = "solc-backend", not(target_arch = "wasm32")))]
use proptest::prelude::*;

use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

struct DualHarness {
    fe_harness: ContractHarness,
    solidity_harness: ContractHarness,
}

struct CaptureResult<'a> {
    fe_capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    fe_used_gas: u64,
    solidity_capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    solidity_used_gas: u64,
    name: &'a str,
    input: &'a [ethabi::Token],
}

impl<'a> CaptureResult<'a> {
    pub fn assert_fe_max_percentage_more_gas(&self, max_percentage: i64) -> &Self {
        let fe_percentage: i64 = (self.fe_used_gas as i64 - self.solidity_used_gas as i64) * 100
            / self.solidity_used_gas as i64;

        assert!(fe_percentage <= max_percentage, "Fe used gas: {}, Solidity used gas: {}, Fe used {}% more gas. Called {} with input: {:?}", self.fe_used_gas, self.solidity_used_gas, fe_percentage, self.name, self.input);
        self
    }

    pub fn assert_perfomed_equal(&self) -> &Self {
        assert_eq!(
            self.fe_capture, self.solidity_capture,
            "Called {} with input: {:?}",
            self.name, self.input
        );
        self
    }

    pub fn assert_return_data_equal(&self) -> &Self {
        if let (evm::Capture::Exit((_, fe_data)), evm::Capture::Exit((_, sol_data))) =
            (&self.fe_capture, &self.solidity_capture)
        {
            assert_eq!(
                fe_data, sol_data,
                "Called {} with input: {:?}",
                self.name, self.input
            )
        }
        self
    }

    #[allow(dead_code)]
    pub fn assert_reverted(&self) -> &Self {
        if !matches!(
            (self.fe_capture.clone(), self.solidity_capture.clone()),
            (
                evm::Capture::Exit((evm::ExitReason::Revert(_), _)),
                evm::Capture::Exit((evm::ExitReason::Revert(_), _))
            )
        ) {
            panic!(
                "Asserted both revert but was: Fe: {:?} Solidity: {:?}",
                self.fe_capture, self.solidity_capture
            )
        }
        self
    }

    pub fn assert_any_success_with_equal_return_data(&self) -> &Self {
        self.assert_any_success().assert_return_data_equal();
        self
    }

    pub fn assert_any_success_or_revert_with_equal_return_data(&self) -> &Self {
        if !(self.both_succeeded() || self.both_reverted()) {
            panic!(
                "Asserted both succeeded or reverted but was: Fe: {:?} Solidity: {:?}",
                self.fe_capture, self.solidity_capture
            )
        } else {
            self.assert_return_data_equal()
        }
    }

    pub fn assert_any_success(&self) -> &Self {
        if !matches!(
            (self.fe_capture.clone(), self.solidity_capture.clone()),
            (
                evm::Capture::Exit((evm::ExitReason::Succeed(_), _)),
                evm::Capture::Exit((evm::ExitReason::Succeed(_), _))
            )
        ) {
            panic!(
                "Asserted both succeeded but was: Fe: {:?} Solidity: {:?}",
                self.fe_capture, self.solidity_capture
            )
        }
        self
    }

    pub fn both_succeeded(&self) -> bool {
        matches!(
            (self.fe_capture.clone(), self.solidity_capture.clone()),
            (
                evm::Capture::Exit((evm::ExitReason::Succeed(_), _)),
                evm::Capture::Exit((evm::ExitReason::Succeed(_), _))
            )
        )
    }

    pub fn both_reverted(&self) -> bool {
        matches!(
            (self.fe_capture.clone(), self.solidity_capture.clone()),
            (
                evm::Capture::Exit((evm::ExitReason::Revert(_), _)),
                evm::Capture::Exit((evm::ExitReason::Revert(_), _))
            )
        )
    }

    #[allow(dead_code)]
    pub fn performed_equal(&self) -> bool {
        self.fe_capture == self.solidity_capture
    }
}

impl<'a> DualHarness {
    pub fn from_fixture(
        executor: &mut Executor,
        fixture: &str,
        contract_name: &str,
        init_params: &[ethabi::Token],
    ) -> DualHarness {
        let fe_harness = test_utils::deploy_contract(
            executor,
            &format!("differential/{fixture}.fe"),
            contract_name,
            init_params,
        );
        let solidity_harness = test_utils::deploy_solidity_contract(
            executor,
            &format!("differential/{fixture}.sol"),
            contract_name,
            init_params,
            true,
        );
        DualHarness {
            fe_harness,
            solidity_harness,
        }
    }

    pub fn capture_call(
        &self,
        executor: &mut Executor,
        name: &'a str,
        input: &'a [ethabi::Token],
    ) -> CaptureResult<'a> {
        let initially_used = executor.used_gas();
        let fe_capture = self.fe_harness.capture_call(executor, name, input);
        let fe_used_gas = executor.used_gas() - initially_used;
        let solidity_capture = self.solidity_harness.capture_call(executor, name, input);
        let solidity_used_gas = executor.used_gas() - fe_used_gas - initially_used;

        CaptureResult {
            fe_capture,
            fe_used_gas,
            solidity_capture,
            solidity_used_gas,
            name,
            input,
        }
    }
}

proptest! {

    #[test]
    #[ignore]
    fn math_u8(val in 0u8..=255, val2 in 0u8..=255) {
        with_executor(&|mut executor| {

            let harness = DualHarness::from_fixture(&mut executor, "math_u8", "Foo", &[]);

            harness.capture_call(&mut executor, "add", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call(&mut executor, "subtract", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call(&mut executor, "divide", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(100);
            harness.capture_call(&mut executor, "multiply", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call(&mut executor, "pow", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(15);
            harness.capture_call(&mut executor, "modulo", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call(&mut executor, "leftshift", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call(&mut executor, "rightshift", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call(&mut executor, "order_of_operation", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call(&mut executor, "invert", &[uint_token(val.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(60);
            harness.capture_call(&mut executor, "bit_and", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(15);
            harness.capture_call(&mut executor, "bit_or", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call(&mut executor, "bit_xor", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(30);
            harness.capture_call(&mut executor, "cast1", &[uint_token(val.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call(&mut executor, "cast2", &[uint_token(val.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(60);
            harness.capture_call(&mut executor, "cast3", &[uint_token(val.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(70);
            harness.capture_call(&mut executor, "sqrt", &[uint_token(val.into())]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(120);
        });
    }

    #[test]
    #[ignore]
    fn math_i8(val in -128i8..=127i8, val2 in -128i8..=127i8, val3 in 0u8..=255) {
        with_executor(&|mut executor| {
            let harness = DualHarness::from_fixture(&mut executor, "math_i8", "Foo", &[]);

            harness.capture_call(&mut executor, "add", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "subtract", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "divide", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "multiply", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "modulo", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "leftshift", &[int_token(val.into()), uint_token(val3.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "rightshift", &[int_token(val.into()), uint_token(val3.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "order_of_operation", &[int_token(val.into()), int_token(val2.into()), uint_token(val3.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "invert", &[int_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast1", &[int_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast2", &[int_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast3", &[int_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "negate", &[int_token(val.into())]).assert_perfomed_equal();
        });
    }

    #[test]
    #[ignore]
    fn storage_and_memory(my_num in 0u64..=100000,
                          my_num2 in 0u8..=255, my_bool in any::<bool>(),
                          my_str in "[0-9]{20}",
                          my_long_string in ".{0,40}",
                          my_num3 in -128i8..=127i8) {
        with_executor(&|mut executor| {


            let harness = DualHarness::from_fixture(&mut executor, "storage_and_memory", "Foo", &[]);

            let data = ethabi::Token::Tuple(vec![
                uint_token(my_num),
                uint_token(my_num2.into()),
                bool_token(my_bool),
                address_token(&my_str),
                int_token(my_num3.into())
            ]);

            harness.capture_call(&mut executor, "set_data", &[data]).assert_any_success_with_equal_return_data().assert_fe_max_percentage_more_gas(200);
            harness.capture_call(&mut executor, "get_data", &[]).assert_perfomed_equal().assert_fe_max_percentage_more_gas(150);

            harness.capture_call(&mut executor, "set_item", &[uint_token(my_num2.into()), int_token(my_num3.into())]).assert_any_success_or_revert_with_equal_return_data();
            harness.capture_call(&mut executor, "get_items", &[]).assert_perfomed_equal();

            harness.capture_call(&mut executor, "set_string", &[string_token(&my_long_string)]).assert_any_success_with_equal_return_data();
            harness.capture_call(&mut executor, "get_string", &[]).assert_perfomed_equal();

            harness.capture_call(&mut executor, "set_range", &[uint_token(my_num2.into()), uint_token(my_num)]).assert_any_success_or_revert_with_equal_return_data();
            harness.capture_call(&mut executor, "get_range", &[]).assert_perfomed_equal().assert_any_success_or_revert_with_equal_return_data();

        });
    }
}
