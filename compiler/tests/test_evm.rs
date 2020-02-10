use std::fs;
use vyper_compiler::evm as compiler;
use vyper_parser::parsers;

fn compile_fixture(name: &str) -> String {
    let src = fs::read_to_string(format!("tests/fixtures/{}", name))
        .expect("Unable to read fixture file.");
    compiler::compile(&src).expect("Unable to compile fixture.")
}

#[cfg(test)]
mod tests {
    use crate::compile_fixture;
    use ethabi;
    use evm;
    use evm_runtime::{CreateScheme, Handler};
    use primitive_types::{H160, U256};
    use std::collections::BTreeMap;

    fn vicinity() -> evm::backend::MemoryVicinity {
        let zero = U256::from_dec_str("0").unwrap();
        let addr = H160::repeat_byte(5);

        evm::backend::MemoryVicinity {
            gas_price: zero.clone(),
            origin: addr.clone(),
            chain_id: zero.clone(),
            block_hashes: Vec::new(),
            block_number: zero.clone(),
            block_coinbase: addr.clone(),
            block_timestamp: zero.clone(),
            block_difficulty: zero.clone(),
            block_gas_limit: U256::MAX,
        }
    }

    #[test]
    fn test_evm_sanity() {
        let state: BTreeMap<H160, evm::backend::MemoryAccount> = BTreeMap::new();
        let config = evm::Config::istanbul();

        let vicinity = vicinity();
        let backend = evm::backend::MemoryBackend::new(&vicinity, state);
        let mut executor = evm::executor::StackExecutor::new(&backend, usize::max_value(), &config);

        let addr = H160::repeat_byte(4);
        let amount = U256::from_dec_str("1000").unwrap();

        executor.deposit(addr, amount);
        assert_eq!(executor.balance(addr), amount);
    }

    #[test]
    fn test_simple_contract() {
        let state: BTreeMap<H160, evm::backend::MemoryAccount> = BTreeMap::new();
        let config = evm::Config::istanbul();

        let vicinity = vicinity();
        let backend = evm::backend::MemoryBackend::new(&vicinity, state);
        let mut executor = evm::executor::StackExecutor::new(&backend, usize::max_value(), &config);

        let addr = H160::repeat_byte(4);
        let amount = U256::from_dec_str("1000").unwrap();
        let zero = U256::from_dec_str("0").unwrap();

        let bytecode =
            hex::decode(compile_fixture("simple_contract.vy")).expect("Unable to decode bytecode");

        executor.deposit(addr, amount);
        if let evm::Capture::Exit(exit) =
            executor.create(addr, CreateScheme::Dynamic, zero, bytecode, None)
        {
            let code_address = exit.1.expect("No contract address.");
            let context = evm::Context {
                address: addr,
                caller: addr,
                apparent_value: zero,
            };
            let selector = "0b1fde7800000000000000000000000000000000000000000000000000000000";
            let argument = "ff00000000000000000000000000000000000000000000000000000000000000";
            let input = hex::decode(format!("{}{}", selector, argument)).unwrap();

            if let evm::Capture::Exit(exit) =
                executor.call(code_address, None, input, None, false, context)
            {
                let expected_output = "ff00000000000000000000000000000000000000000000000000000000000000";
                assert_eq!(exit.0, evm::ExitReason::Succeed(evm::ExitSucceed::Returned));
                assert_eq!(exit.1, hex::decode(expected_output).unwrap(), "Unexpected output.")
            }
        } else {
            assert!(false, "Failed to create contract.")
        }
    }
}
