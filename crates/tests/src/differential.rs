//! Tests that check for differences between Solidity and Fe implementations of similar contracts
#![cfg(feature = "solc-backend")]
use proptest::prelude::*;

use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

struct DualHarness {
    fe_harness: ContractHarness,
    solidity_harness: ContractHarness,
}

struct CaptureResult<'a> {
    fe_capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    solidity_capture: evm::Capture<(evm::ExitReason, Vec<u8>), std::convert::Infallible>,
    name: &'a str,
    input: &'a [ethabi::Token],
}

impl<'a> CaptureResult<'a> {
    pub fn assert_perfomed_equal(&self) {
        assert_eq!(
            self.fe_capture, self.solidity_capture,
            "Called {} with input: {:?}",
            self.name, self.input
        )
    }

    pub fn assert_reverted(&self) {
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
    }

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
            &format!("differential/{}.fe", fixture),
            contract_name,
            init_params,
        );
        let solidity_harness = test_utils::deploy_solidity_contract(
            executor,
            &format!("differential/{}.sol", fixture),
            contract_name,
            init_params,
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
        let fe_capture = self.fe_harness.capture_call(executor, name, input);
        let solidity_capture = self.solidity_harness.capture_call(executor, name, input);

        CaptureResult {
            fe_capture,
            solidity_capture,
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

            harness.capture_call(&mut executor, "add", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "subtract", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "divide", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "multiply", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "pow", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "modulo", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "leftshift", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "rightshift", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "order_of_operation", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "invert", &[uint_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "bit_and", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "bit_or", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "bit_xor", &[uint_token(val.into()), uint_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast1", &[uint_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast2", &[uint_token(val.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "cast3", &[uint_token(val.into())]).assert_perfomed_equal();
        });
    }

    #[test]
    #[ignore]
    fn math_i8(val in -128i8..=127i8, val2 in -128i8..=127i8, val3 in 0u8..=255, val4 in 0u8..=255) {
        with_executor(&|mut executor| {
            let harness = DualHarness::from_fixture(&mut executor, "math_i8", "Foo", &[]);

            harness.capture_call(&mut executor, "add", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "subtract", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "divide", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            harness.capture_call(&mut executor, "multiply", &[int_token(val.into()), int_token(val2.into())]).assert_perfomed_equal();
            let inputs = &[int_token(val3.into()), uint_token(val4.into())];
            let result = harness.capture_call(&mut executor, "pow", inputs);
            if !result.performed_equal() {
                // If they didn't performed equal then we further assert that at least both reverted. Solidity reverts
                // with zero revert data when a pow operation overflows whereas Fe uses a proper revert error.
                // We tolerate the divergence and assume its a TODO on the Solidity side :)
                result.assert_reverted();
            }

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
}
