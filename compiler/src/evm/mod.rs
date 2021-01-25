//! Fe to EVM compiler.

use crate::errors::CompileError;
use crate::types::{
    Bytecode,
    NamedBytecodeContracts,
    NamedYulContracts,
    YulIr,
};

/// Compiles Fe to bytecode. It uses Yul as an intermediate representation.
pub fn compile(mut contracts: NamedYulContracts) -> Result<NamedBytecodeContracts, CompileError> {
    contracts
        .drain()
        .map(|(name, yul_src)| compile_single_contract(yul_src).map(|bytecode| (name, bytecode)))
        .collect::<Result<NamedBytecodeContracts, _>>()
}

fn compile_single_contract(yul_src: YulIr) -> Result<Bytecode, CompileError> {
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp.replace("{src}", &yul_src);
    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)?;

    let bytecode = output["contracts"]["input.yul"]["Contract"]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    if bytecode == "null" {
        return Err(CompileError::str(output.to_string()));
    }

    Ok(bytecode)
}

#[test]
fn test_solc_sanity() {
    let yul_src = "{ sstore(0,0) }";
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp.replace("{src}", &yul_src);

    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output).unwrap();

    let bytecode = output["contracts"]["input.yul"]["object"]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    assert_eq!(bytecode, "6000600055", "incorrect bytecode",);
}
