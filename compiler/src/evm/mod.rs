//! Fe to EVM compiler.

use crate::errors::CompileError;
use crate::yul;

pub struct CompilerOutput {
    pub tokens: String,
    pub ast: String,
    pub yul: String,
    pub bytecode: String,
}

/// Compiles Fe to bytecode. It uses Yul as an intermediate representation.
pub fn compile(src: &str) -> Result<CompilerOutput, CompileError> {
    let solc_temp = include_str!("solc_temp.json");
    let yul_output = yul::compile(src)?;
    let yul_src = yul_output.yul.replace("\"", "\\\"");
    let input = solc_temp.replace("{src}", &yul_src);
    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)?;

    let bytecode = output["contracts"]["input.yul"]["Contract"]["evm"]["bytecode"]["object"]
        .to_string()
        .replace("\"", "");
    if bytecode == "null" {
        return Err(CompileError::str(output.to_string()));
    }

    Ok(CompilerOutput {
        ast: yul_output.ast,
        bytecode,
        tokens: yul_output.tokens,
        yul: yul_src,
    })
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
