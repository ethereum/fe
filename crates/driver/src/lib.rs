use fe_common::diagnostics::Diagnostic;
use fe_common::files::SourceFileId;
use fe_parser::parse_file;
#[cfg(feature = "solc-backend")]
use serde_json::Value;
use std::collections::HashMap;

/// The artifacts of a compiled module.
pub struct CompiledModule {
    pub src_ast: String,
    pub lowered_ast: String,
    pub contracts: HashMap<String, CompiledContract>,
}

/// The artifacts of a compiled contract.
pub struct CompiledContract {
    pub json_abi: String,
    pub yul: String,
    #[cfg(feature = "solc-backend")]
    pub bytecode: String,
}

#[derive(Debug)]
pub struct CompileError(pub Vec<Diagnostic>);

/// Compiles the given Fe source code to all targets.
///
/// If `with_bytecode` is set to false, the compiler will skip the final Yul ->
/// Bytecode pass. This is useful when debugging invalid Yul code.
pub fn compile(
    src: &str,
    file_id: SourceFileId,
    _with_bytecode: bool,
    _optimize: bool,
) -> Result<CompiledModule, CompileError> {
    // parse source

    let mut errors = vec![];

    let (fe_module, parser_diagnostics) = parse_file(src, file_id).map_err(CompileError)?;
    errors.extend(parser_diagnostics.into_iter());

    // analyze source code
    let analysis = match fe_analyzer::analyze(&fe_module, file_id) {
        Ok(_) if !errors.is_empty() => return Err(CompileError(errors)),
        Ok(analysis) => analysis,
        Err(err) => {
            errors.extend(err.0.into_iter());
            return Err(CompileError(errors));
        }
    };

    assert!(errors.is_empty());

    // build abi
    let json_abis = fe_abi::build(&analysis, &fe_module).expect("failed to generate abi");

    // lower the AST
    let lowered_fe_module = fe_lowering::lower(&analysis, fe_module.clone());

    // analyze the lowered AST
    let analysis =
        fe_analyzer::analyze(&lowered_fe_module, file_id).expect("failed to analyze lowered AST");

    // compile to yul
    let yul_contracts = fe_yulgen::compile(&analysis, &lowered_fe_module);

    // compile to bytecode if required
    #[cfg(feature = "solc-backend")]
    let bytecode_contracts = if _with_bytecode {
        match fe_yulc::compile(yul_contracts.clone(), _optimize) {
            Err(error) => {
                for error in serde_json::from_str::<Value>(&error.0)
                    .expect("unable to deserialize json output")["errors"]
                    .as_array()
                    .expect("errors not an array")
                {
                    eprintln!(
                        "Error: {}",
                        error["formattedMessage"]
                            .as_str()
                            .expect("error value not a string")
                            .replace("\\\n", "\n")
                    )
                }
                panic!("Yul compilation failed with the above errors")
            }
            Ok(contracts) => contracts,
        }
    } else {
        std::collections::HashMap::new()
    };

    // combine all of the named contract maps
    let contracts = json_abis
        .keys()
        .map(|name| {
            (
                name.to_owned(),
                CompiledContract {
                    json_abi: json_abis[name].to_owned(),
                    yul: yul_contracts[name].to_owned(),
                    #[cfg(feature = "solc-backend")]
                    bytecode: if _with_bytecode {
                        bytecode_contracts[name].to_owned()
                    } else {
                        "".to_string()
                    },
                },
            )
        })
        .collect::<HashMap<_, _>>();

    Ok(CompiledModule {
        src_ast: format!("{:#?}", fe_module),
        lowered_ast: format!("{:#?}", lowered_fe_module),
        contracts,
    })
}
