use crate::errors::CompileError;
use crate::yul;
use solc;
use serde_json;

pub fn compile(src: &str) -> Result<String, CompileError> {{
    let solc_temp = include_str!("solc_input.json");
    let yul_src = yul::compile(src)?.replace("\"", "\\\"");
    let input = solc_temp.replace("{src}", &yul_src);

    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)?;

    let asm = output["contracts"]["input.yul"]["object"]["evm"]["assembly"].clone();
    if asm.to_string() == "null" {
        return Err(CompileError::str(output.to_string()));
    }

    Ok(asm.to_string())
}}
