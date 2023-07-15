use bytes::Bytes;
use colored::Colorize;
use revm::primitives::{AccountInfo, Bytecode, Env, ExecutionResult, TransactTo, B160, U256};
use std::fmt::Display;

#[derive(Debug, Default)]
pub struct TestSink {
    success_count: usize,
    failure_details: Vec<String>,
}

impl TestSink {
    pub fn test_count(&self) -> usize {
        self.failure_count() + self.success_count()
    }

    pub fn failure_count(&self) -> usize {
        self.failure_details.len()
    }

    pub fn success_count(&self) -> usize {
        self.success_count
    }

    pub fn insert_failure(&mut self, name: &str, reason: &str) {
        self.failure_details
            .push(format!("{} ({})", name, reason.red()))
    }

    pub fn inc_success_count(&mut self) {
        self.success_count += 1
    }

    pub fn failure_details(&self) -> String {
        self.failure_details.join("\n")
    }
}

impl Display for TestSink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.failure_count() != 0 {
            writeln!(f, "{}", self.failure_details())?;
            writeln!(f)?;
        }

        let test_description = |n: usize, status: &dyn Display| -> String {
            if n == 1 {
                format!("1 test {status}")
            } else {
                format!("{n} tests {status}")
            }
        };

        write!(
            f,
            "{}; ",
            test_description(self.success_count(), &"passed".green())
        )?;
        write!(
            f,
            "{}; ",
            test_description(self.failure_count(), &"failed".red())
        )?;
        write!(f, "{}", test_description(self.test_count(), &"executed"))
    }
}

pub fn execute(name: &str, bytecode: &str, sink: &mut TestSink) -> bool {
    let bytecode = Bytecode::new_raw(Bytes::copy_from_slice(&hex::decode(bytecode).unwrap()));

    let mut database = revm::InMemoryDB::default();
    let test_address = B160::from(42);
    let test_info = AccountInfo::new(U256::ZERO, 0, bytecode);
    database.insert_account_info(test_address, test_info);

    let mut env = Env::default();
    env.tx.transact_to = TransactTo::Call(test_address);

    let mut evm = revm::new();
    evm.env = env;
    evm.database(&mut database);
    let result = evm.transact_commit().expect("evm failure");

    if result.is_success() {
        sink.inc_success_count();
        true
    } else if let ExecutionResult::Revert { gas_used, output } = result {
        sink.insert_failure(
            name,
            &format!(
                "Reverted gas used: {} output: {}",
                gas_used,
                hex::encode(output)
            ),
        );
        false
    } else {
        panic!("test halted")
    }
}
