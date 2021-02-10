//! Fe to Yul compiler.

use crate::errors::CompileError;
use crate::types::{
    FeModuleAst,
    NamedYulContracts,
};
use fe_analyzer::Context;

mod constructor;
mod mappers;
mod names;
mod operations;
mod runtime;
mod utils;

/// Compiles Fe source code to Yul.
pub fn compile(context: Context, module: &FeModuleAst) -> Result<NamedYulContracts, CompileError> {
    Ok(mappers::module::module(&context, module)?
        .drain()
        .map(|(name, object)| (name, object.to_string().replace("\"", "\\\"")))
        .collect::<NamedYulContracts>())
}
