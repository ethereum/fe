//! Modules for compiling Fe and building ABIs.

use crate::errors::{
    CompileError,
    ErrorKind,
};
use crate::types::{
    CompiledContract,
    CompiledModule,
    NamedContracts,
};
use fe_common::files::SourceFileId;
use fe_parser::parse_file;
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
    src: &str,
    file_id: SourceFileId,
    _with_bytecode: bool,
    _optimize: bool,
) -> Result<CompiledModule, CompileError> {
    // parse source

    let fe_module = parse_file(src, file_id).map_err(|diags| CompileError {
        errors: vec![ErrorKind::Parser(diags)],
    })?;

    // analyze source code
    let context = fe_analyzer::analyze(&fe_module)?;

    // build abi
    let json_abis = abi::build(&context, &fe_module)?;

    // lower the AST
    let lowered_fe_module = lowering::lower(&context, fe_module.clone());

    // analyze the lowered AST
    let context = fe_analyzer::analyze(&lowered_fe_module)
        .map_err(|error| CompileError::str(&error.format_user(src)))?;

    // compile to yul
    let yul_contracts = yul::compile(&context, &lowered_fe_module);

    // compile to bytecode if required
    #[cfg(feature = "solc-backend")]
    let bytecode_contracts = if _with_bytecode {
        match evm::compile(yul_contracts.clone(), _optimize) {
            Err(error) => {
                match &error.errors[0] {
                    ErrorKind::Str(string) => eprintln!("Error: {}", string),
                    err => eprintln!("Error: {:?}", err),
                }
                panic!("Yul compilation failed.")
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
        .collect::<NamedContracts>();

    Ok(CompiledModule {
        src_ast: format!("{:#?}", fe_module),
        lowered_ast: format!("{:#?}", lowered_fe_module),
        contracts,
    })
}
