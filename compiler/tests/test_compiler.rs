use std::fs;
use vyper_compiler::yul as compiler;
use vyper_parser::parsers;

struct Fixture {
    vyp: String,
    yul: String,
}

fn read_fixture(path: &str) -> Fixture {
    let contents = fs::read_to_string(path).expect("Unable to read fixture file.");
    let split_contents: Vec<&str> = contents.split("---").collect();

    Fixture {
        vyp: String::from(split_contents[0]),
        yul: String::from(split_contents[1]),
    }
}

#[cfg(test)]
mod tests {
    use ethabi;
    use evm;
    use primitive_types::{U256,H160};
    use std::collections::BTreeMap;
    use evm_runtime::Handler;

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
            block_gas_limit: U256::MAX
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
}
