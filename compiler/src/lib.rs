//! Modules for compiling Fe and building ABIs.

use crate::errors::CompileError;
use crate::types::{
    CompiledContract,
    CompiledModule,
    FeSrc,
    NamedContracts,
};

pub mod abi;
pub mod errors;
#[cfg(feature = "solc-backend")]
pub mod evm;
pub mod lowering;
pub mod types;
pub mod yul;

/// Compiles the given Fe source code to all targets.
///
/// If `with_bytecode` is set to false, the compiler will skip the final Yul ->
/// Bytecode pass. This is useful when debugging invalid Yul code.
pub fn compile(
    src: FeSrc,
    _with_bytecode: bool,
    _optimize: bool,
) -> Result<CompiledModule, CompileError> {
    // parse source
    let fe_tokens = fe_parser::get_parse_tokens(src)?;
    let fe_module = fe_parser::parsers::file_input(&fe_tokens[..])
        .map_err(|error| CompileError::str(&error.format_user(src)))?
        .1
        .kind;

    // analyze source code
    let context = fe_analyzer::analyze(&fe_module)
        .map_err(|error| CompileError::str(&error.format_user(src)))?;

    // build abi
    let json_abis = abi::build(&context, &fe_module)?;

    // lower the AST
    let lowered_fe_module = lowering::lower(&context, &fe_module);

    // analyze the lowered AST
    let context = fe_analyzer::analyze(&lowered_fe_module)
        .map_err(|error| CompileError::str(&error.format_user(src)))?;

    // compile to yul
    let yul_contracts = yul::compile(&context, &lowered_fe_module);

    // compile to bytecode if required
    #[cfg(feature = "solc-backend")]
    let bytecode_contracts = if _with_bytecode {
        match evm::compile(yul_contracts.clone(), _optimize) {
            Err(error) => panic!("Yul compilation failed: {}", error),
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
        .collect::<NamedContracts>();

    Ok(CompiledModule {
        fe_tokens: format!("{:#?}", fe_tokens),
        fe_ast: format!("{:#?}", fe_module),
        contracts,
    })
}
