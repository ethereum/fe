//! Fe to EVM compiler.

use crate::errors::CompileError;
use crate::types::{Bytecode, NamedBytecodeContracts, NamedYulContracts, YulIr};

/// Compile a map of Yul contracts to a map of bytecode contracts.
pub fn compile(
    mut contracts: NamedYulContracts,
    optimize: bool,
) -> Result<NamedBytecodeContracts, CompileError> {
    contracts
        .drain()
        .map(|(name, yul_src)| {
            compile_single_contract(&name, yul_src, optimize).map(|bytecode| (name, bytecode))
        })
        .collect::<Result<NamedBytecodeContracts, _>>()
}

/// Compiles a single Yul contract to bytecode.
pub fn compile_single_contract(
    name: &str,
    yul_src: YulIr,
    optimize: bool,
) -> Result<Bytecode, CompileError> {
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp
        .replace("{optimizer_enabled}", &optimize.to_string())
        .replace("{src}", &yul_src);
    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)?;

    let bytecode = output["contracts"]["input.yul"][name]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    if bytecode == "null" {
        return Err(CompileError::str(&output.to_string()));
    }

    Ok(bytecode)
}

#[test]
fn test_solc_sanity() {
    let yul_src = "{ sstore(0,0) }";
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp
        .replace("{optimizer_enabled}", "false")
        .replace("{src}", &yul_src);

    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output).unwrap();

    let bytecode = output["contracts"]["input.yul"]["object"]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    assert_eq!(bytecode, "6000600055", "incorrect bytecode",);
}
