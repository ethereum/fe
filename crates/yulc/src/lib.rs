//! Fe to EVM compiler.

use fe_yulgen::{NamedYulContracts, YulIr};
use std::collections::HashMap;

/// The name of a Fe contract.
pub type ContractName = String;
/// The bytecode of a contract as string object.
pub type Bytecode = String;
/// A mapping of contract names and their bytecode.
pub type NamedBytecodeContracts = HashMap<ContractName, Bytecode>;

#[derive(Debug)]
pub struct YulcError(pub String);

/// Compile a map of Yul contracts to a map of bytecode contracts.
pub fn compile(
    mut contracts: NamedYulContracts,
    optimize: bool,
) -> Result<NamedBytecodeContracts, YulcError> {
    contracts
        .drain()
        .map(|(name, yul_src)| {
            compile_single_contract(&name, yul_src, optimize).map(|bytecode| (name, bytecode))
        })
        .collect::<Result<NamedBytecodeContracts, _>>()
}

#[cfg(feature = "solc-backend")]
/// Compiles a single Yul contract to bytecode.
pub fn compile_single_contract(
    name: &str,
    yul_src: YulIr,
    optimize: bool,
) -> Result<Bytecode, YulcError> {
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp
        .replace("{optimizer_enabled}", &optimize.to_string())
        .replace("{src}", &yul_src);
    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)
        .map_err(|_| YulcError("JSON serialization error".into()))?;

    let bytecode = output["contracts"]["input.yul"][name]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    if bytecode == "null" {
        return Err(YulcError(output.to_string()));
    }

    Ok(bytecode)
}

#[cfg(not(feature = "solc-backend"))]
/// Compiles a single Yul contract to bytecode.
pub fn compile_single_contract(
    _name: &str,
    _yul_src: YulIr,
    _optimize: bool,
) -> Result<Bytecode, YulcError> {
    // This is ugly, but required (as far as I can tell) to make
    // `cargo test --workspace` work without solc.
    panic!("fe-yulc requires 'solc-backend' feature")
}

#[cfg(feature = "solc-backend")]
#[test]
fn test_solc_sanity() {
    let yul_src = "{ sstore(0,0) }";
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp
        .replace("{optimizer_enabled}", "false")
        .replace("{src}", yul_src);

    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output).unwrap();

    let bytecode = output["contracts"]["input.yul"]["object"]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");

    assert_eq!(bytecode, "6000600055", "incorrect bytecode",);
}
