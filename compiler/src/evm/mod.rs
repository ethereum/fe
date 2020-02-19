use crate::errors::CompileError;
use crate::yul;
use serde_json;
use solc;

/// Compiles Yul source code into bytecode
pub fn compile(src: &str) -> Result<String, CompileError> {
    let solc_temp = include_str!("solc_temp.json");
    let yul_src = yul::compile(src)?.replace("\"", "\\\"");
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
