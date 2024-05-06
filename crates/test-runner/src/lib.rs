use colored::Colorize;
use ethabi::{Event, Hash, RawLog};
use indexmap::IndexMap;
use revm::primitives::{
    AccountInfo, Address, Bytecode, Bytes, Env, ExecutionResult, TransactTo, B256, U256,
};
use std::{fmt::Display, str::FromStr};

pub use ethabi;

#[derive(Debug)]
pub struct TestSink {
    success_count: usize,
    failure_details: Vec<String>,
    logs_details: Vec<String>,
    collect_logs: bool,
}

impl TestSink {
    pub fn new(collect_logs: bool) -> Self {
        Self {
            success_count: 0,
            failure_details: vec![],
            logs_details: vec![],
            collect_logs,
        }
    }

    pub fn test_count(&self) -> usize {
        self.failure_count() + self.success_count()
    }

    pub fn failure_count(&self) -> usize {
        self.failure_details.len()
    }

    pub fn logs_count(&self) -> usize {
        self.logs_details.len()
    }

    pub fn success_count(&self) -> usize {
        self.success_count
    }

    pub fn insert_failure(&mut self, name: &str, reason: &str) {
        self.failure_details
            .push(format!("{}\n{}", name, reason.red()))
    }

    pub fn insert_logs(&mut self, name: &str, logs: &str) {
        if self.collect_logs {
            self.logs_details.push(format!(
                "{} produced the following logs:\n{}\n",
                name,
                logs.bright_yellow()
            ))
        }
    }

    pub fn inc_success_count(&mut self) {
        self.success_count += 1
    }

    pub fn failure_details(&self) -> String {
        self.failure_details.join("\n")
    }

    pub fn logs_details(&self) -> String {
        self.logs_details.join("\n")
    }
}

impl Display for TestSink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.logs_count() != 0 {
            writeln!(f, "{}", self.logs_details())?;
            writeln!(f)?;
        }

        if self.failure_count() != 0 {
            writeln!(f, "{}", self.failure_details())?;
            writeln!(f)?;
            if self.collect_logs {
                writeln!(f, "note: failed tests do not produce logs")?;
                writeln!(f)?;
            }
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

pub fn execute(name: &str, events: &[Event], bytecode: &str, sink: &mut TestSink) -> bool {
    let events: IndexMap<_, _> = events
        .iter()
        .map(|event| (event.signature(), event))
        .collect();
    let bytecode = Bytecode::new_raw(Bytes::copy_from_slice(&hex::decode(bytecode).unwrap()));

    let mut database = revm::InMemoryDB::default();
    let test_address = Address::from_str("0000000000000000000000000000000000000042").unwrap();
    let test_info = AccountInfo::new(U256::ZERO, 0, B256::default(), bytecode);
    database.insert_account_info(test_address, test_info);

    let mut env = Env::default();
    env.tx.transact_to = TransactTo::Call(test_address);

    let builder = revm::EvmBuilder::default()
        .with_db(database)
        .with_env(Box::new(env));
    let mut evm = builder.build();
    let result = evm.transact_commit().expect("evm failure");

    if let ExecutionResult::Success { logs, .. } = result {
        let logs: Vec<_> = logs
            .iter()
            .map(|log| {
                if let Some(Some(event)) = log
                    .topics()
                    .first()
                    .map(|sig| events.get(&Hash::from_slice(sig.as_slice())))
                {
                    let topics = log
                        .topics()
                        .iter()
                        .map(|topic| Hash::from_slice(topic.as_slice()))
                        .collect();
                    let data = log.data.data.clone().to_vec();
                    let raw_log = RawLog { topics, data };
                    if let Ok(parsed_event) = event.parse_log(raw_log) {
                        format!(
                            "  {} emitted by {} with the following parameters [{}]",
                            event.name,
                            log.address,
                            parsed_event
                                .params
                                .iter()
                                .map(|param| format!("{}: {}", param.name, param.value))
                                .collect::<Vec<String>>()
                                .join(", "),
                        )
                    } else {
                        format!("  {:?}", log)
                    }
                } else {
                    format!("  {:?}", log)
                }
            })
            .collect();

        if !logs.is_empty() {
            sink.insert_logs(name, &logs.join("\n"))
        }

        sink.inc_success_count();
        true
    } else if let ExecutionResult::Revert { output, .. } = result {
        sink.insert_failure(
            name,
            &if output.is_empty() {
                "  reverted".to_string()
            } else {
                format!(
                    "  reverted with the following output: {}",
                    hex::encode(output)
                )
            },
        );
        false
    } else {
        panic!("test halted")
    }
}
