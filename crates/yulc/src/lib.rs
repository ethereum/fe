use indexmap::map::IndexMap;

#[derive(Debug)]
pub struct YulcError(pub String);

/// Compile a map of Yul contracts to a map of bytecode contracts.
///
/// Returns a `contract_name -> hex_encoded_bytecode` map.
pub fn compile(
    contracts: impl Iterator<Item = (impl AsRef<str>, impl AsRef<str>)>,
    optimize: bool,
) -> Result<IndexMap<String, String>, YulcError> {
    contracts
        .map(|(name, yul_src)| {
            compile_single_contract(name.as_ref(), yul_src.as_ref(), optimize)
                .map(|bytecode| (name.as_ref().to_string(), bytecode))
        })
        .collect()
}

#[cfg(feature = "solc-backend")]
/// Compiles a single Yul contract to bytecode.
pub fn compile_single_contract(
    name: &str,
    yul_src: &str,
    optimize: bool,
) -> Result<String, YulcError> {
    let solc_temp = include_str!("solc_temp.json");
    let input = solc_temp
        .replace("{optimizer_enabled}", &optimize.to_string())
        .replace("{src}", yul_src);
    let raw_output = solc::compile(&input);
    let output: serde_json::Value = serde_json::from_str(&raw_output)
        .map_err(|_| YulcError("JSON serialization error".into()))?;

    let bytecode = output["contracts"]["input.yul"][name]["evm"]["bytecode"]["object"]
        .to_string()
        .replace('"', "");

    if bytecode == "null" {
        return Err(YulcError(output.to_string()));
    }

    Ok(bytecode)
}

#[cfg(not(feature = "solc-backend"))]
/// Compiles a single Yul contract to bytecode.
pub fn compile_single_contract(
    _name: &str,
    _yul_src: &str,
    _optimize: bool,
) -> Result<String, YulcError> {
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
        .replace('"', "");

    // solc 0.8.4: push1 0; push1 0; sstore  "6000600055"
    // solc 0.8.7: push1 0; dup1;    sstore  "60008055"
    assert_eq!(bytecode, "60008055", "incorrect bytecode",);
}
