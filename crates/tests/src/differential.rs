//! Tests that check for differences between Solidity and Fe implementations of similar contracts
#![cfg(feature = "solc-backend")]
use proptest::prelude::*;

// use fe_compiler_test_utils::*;
// use fe_compiler_test_utils::{self as test_utils};
use fevm::{Caller, Contract, CallResult, Return, Fevm, ethabi, revm::TransactOut, conversion::*};
use crate::{DIFF_CONTRACTS, DIFF_VM};
// struct DualHarness {
//     fe_harness: ContractHarness,
//     solidity_harness: ContractHarness,
// }

struct Harness<'a> {
   pub fe: Contract<'a>,
   pub sol: Contract<'a>
}

impl<'a, 'b: 'a> Harness<'b> {
    pub fn capture_call(
        &self,
        name: &'a str,
        input: &'a [ethabi::Token],
        caller: &Caller,
    ) -> CaptureResult<'a> {
        let fe_result = self.fe.capture_call(name, input, caller);
     
        let sol_result = self.sol.capture_call(name, input, caller);
      

        CaptureResult::<'a> {
            fe_result,
            sol_result,
            name,
            input,
        }
    }
}
struct CaptureResult<'a> {
    fe_result: CallResult,
    sol_result: CallResult,
    name: &'a str,
    input: &'a [ethabi::Token],
}

impl<'a> CaptureResult<'a> {

    pub fn fe_used_gas(&self) -> u64 {
        self.fe_result.2
    }

    pub fn solidity_used_gas(&self) -> u64 {
        self.sol_result.2
    }


    pub fn ret_types_match(&self) -> bool {
        self.fe_result.0 == self.sol_result.0
    }

    pub fn ret_data_match(&self) -> bool {
        match &self.fe_result.1 {
            TransactOut::None => {
                if let TransactOut::None = &self.sol_result.1 {
                    return true;
                }
                return false;
            },
            TransactOut::Call(data) => {
                if let TransactOut::Call(sol_data) = &self.sol_result.1 {
                    return data == sol_data;
                }
                return false;
                
            },
            _ => {
                return false;
            }
        }
    }
    pub fn assert_fe_max_percentage_more_gas(&self, max_percentage: i64) -> &Self {
        let fe_percentage: i64 = (self.fe_used_gas() as i64 - self.solidity_used_gas() as i64) * 100
            / self.solidity_used_gas() as i64;

        assert!(fe_percentage <= max_percentage, "Fe used gas: {}, Solidity used gas: {}, Fe used {}% more gas. Called {} with input: {:?}", self.fe_used_gas(), self.solidity_used_gas(), fe_percentage, self.name, self.input);
        self
    }
    pub fn assert_gas_equal(&self) -> &Self {
        assert_eq!(&self.fe_result.2, &self.sol_result.2);
        self
    }

    pub fn assert_perfomed_equal(&self) -> &Self {
        self.assert_return_data_equal().assert_both_fail_or_success()
    }

    pub fn assert_return_data_equal(&self) -> &Self {
        assert!(self.ret_data_match());
        self
    }

    pub fn fe_success(&self) -> bool {
        match &self.fe_result.0 {
            Return::Continue |
            Return::Stop |
            Return::Return |
            Return::SelfDestruct => {
                true
            },
            _ => {
                false
            }
        }
    }

    pub fn sol_success(&self) -> bool {
        match &self.sol_result.0 {
            Return::Continue |
            Return::Stop |
            Return::Return |
            Return::SelfDestruct => {
                true
            },
            _ => {
                false
            }
        }
    }

    pub fn both_succeeded(&self) -> bool {
        let is_fe_success = self.fe_success();

        let is_sol_success = self.sol_success();

        return is_sol_success && is_fe_success;
    }

    pub fn both_reverted(&self) -> bool {
        let is_fe_success = self.fe_success();

        let is_sol_success = self.sol_success();

        return (!is_sol_success && !is_fe_success)
    }

    pub fn assert_both_fail_or_success(&self) -> &Self {
        assert!(self.both_reverted() || self.both_succeeded());
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
                self.fe_result, self.sol_result
            )
        } else {
            self.assert_return_data_equal()
        }
    }

    pub fn assert_any_success(&self) -> &Self {
        if !(self.sol_success() || self.fe_success()) {
            panic!(
                "Asserted both succeeded but was: Fe: {:?} Solidity: {:?}",
                self.fe_result, self.sol_result
            )
        }
        self
    }
}


proptest! {

    #[test]
    fn math_u8(val in 0u8..=255, val2 in 0u8..=255) {
    
            let alice = Caller::random();
            let fevm = &DIFF_VM;
            fevm.create_account(&alice, 2000_u64);
            let fe_contract = DIFF_CONTRACTS[1].0.clone();
            let fe_contract = fe_contract.deploy(&alice, &[]);
            let sol_contract = DIFF_CONTRACTS[1].1.clone();
            let sol_contract = sol_contract.deploy(&alice, &[]);
            let harness = Harness {
                fe: fe_contract,
                sol: sol_contract,
            };
  
            harness.capture_call("add", &[uint_token(val.into()), uint_token(val2.into())], &alice)
            .assert_return_data_equal()
            .assert_fe_max_percentage_more_gas(5); 
         

            harness.capture_call("add", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call("subtract", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call("divide", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(100);
            harness.capture_call("multiply", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(5);
            harness.capture_call("pow", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(15);
            harness.capture_call("modulo", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call("leftshift", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call("rightshift", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(25);
            harness.capture_call("order_of_operation", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call("invert", &[uint_token(val.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(60);
            harness.capture_call( "bit_and", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(15);
            harness.capture_call( "bit_or", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call( "bit_xor", &[uint_token(val.into()), uint_token(val2.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(30);
            harness.capture_call( "cast1", &[uint_token(val.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(50);
            harness.capture_call( "cast2", &[uint_token(val.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(60);
            harness.capture_call( "cast3", &[uint_token(val.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(70);
            harness.capture_call( "sqrt", &[uint_token(val.into())], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(120);
    }

    #[test]
    fn math_i8(val in -128i8..=127i8, val2 in -128i8..=127i8, val3 in 0u8..=255) {
            let alice = Caller::random();
            let fevm = &DIFF_VM;
            fevm.create_account(&alice, 2000_u64);
            let fe_contract = DIFF_CONTRACTS[0].0.clone();
            let fe_contract = fe_contract.deploy(&alice, &[]);
            let sol_contract = DIFF_CONTRACTS[0].1.clone();
            let sol_contract = sol_contract.deploy(&alice, &[]);
            let harness = Harness {
                fe: fe_contract,
                sol: sol_contract,
            };

            harness.capture_call("add", &[int_token(val.into()), int_token(val2.into())], &alice).assert_perfomed_equal();
            harness.capture_call("subtract", &[int_token(val.into()), int_token(val2.into())], &alice).assert_perfomed_equal();
            harness.capture_call("divide", &[int_token(val.into()), int_token(val2.into())], &alice).assert_perfomed_equal();
            harness.capture_call("multiply", &[int_token(val.into()), int_token(val2.into())], &alice).assert_perfomed_equal();
            harness.capture_call("modulo", &[int_token(val.into()), int_token(val2.into())], &alice).assert_perfomed_equal();
            harness.capture_call("leftshift", &[int_token(val.into()), uint_token(val3.into())], &alice).assert_perfomed_equal();
            harness.capture_call("rightshift", &[int_token(val.into()), uint_token(val3.into())], &alice).assert_perfomed_equal();
            harness.capture_call("order_of_operation", &[int_token(val.into()), int_token(val2.into()), uint_token(val3.into())], &alice).assert_perfomed_equal();
            harness.capture_call("invert", &[int_token(val.into())], &alice).assert_perfomed_equal();
            harness.capture_call("cast1", &[int_token(val.into())], &alice).assert_perfomed_equal();
            harness.capture_call("cast2", &[int_token(val.into())], &alice).assert_perfomed_equal();
            harness.capture_call("cast3", &[int_token(val.into())], &alice).assert_perfomed_equal();
            harness.capture_call("negate", &[int_token(val.into())], &alice).assert_perfomed_equal();
    }

    #[test]
    fn storage_and_memory(my_num in 0u64..=100000,
                          my_num2 in 0u8..=255, my_bool in any::<bool>(),
                          my_str in "[0-9]{20}",
                          my_long_string in ".{0,40}",
                          my_num3 in -128i8..=127i8) {
    

            let alice = Caller::random();
            let fevm = &DIFF_VM;
            fevm.create_account(&alice, 2000_u64);
            let fe_contract = DIFF_CONTRACTS[2].0.clone();
            let fe_contract = fe_contract.deploy(&alice, &[]);
            let sol_contract = DIFF_CONTRACTS[2].1.clone();
            let sol_contract = sol_contract.deploy(&alice, &[]);
            let harness = Harness {
                fe: fe_contract,
                sol: sol_contract,
            };

            let data = ethabi::Token::Tuple(vec![
                uint_token(my_num),
                uint_token(my_num2.into()),
                bool_token(my_bool),
                address_token_from_str(&my_str),
                int_token(my_num3.into())
            ]);

            harness.capture_call("set_data", &[data], &alice).assert_any_success_with_equal_return_data().assert_fe_max_percentage_more_gas(200);
            harness.capture_call("get_data", &[], &alice).assert_perfomed_equal().assert_fe_max_percentage_more_gas(150);

            harness.capture_call("set_item", &[uint_token(my_num2.into()), int_token(my_num3.into())], &alice).assert_any_success_or_revert_with_equal_return_data();
            // Waiting on a fix for https://github.com/ethereum/fe/pull/581
            //harness.capture_call(&mut executor, "get_items", &[]).assert_perfomed_equal();

            harness.capture_call("set_string", &[string_token(&my_long_string)], &alice).assert_any_success_with_equal_return_data();
            harness.capture_call("get_string", &[], &alice).assert_perfomed_equal();

            harness.capture_call("set_range", &[uint_token(my_num2.into()), uint_token(my_num)], &alice).assert_any_success_or_revert_with_equal_return_data();
            harness.capture_call("get_range", &[], &alice).assert_perfomed_equal().assert_any_success_or_revert_with_equal_return_data();

    
    }
}
