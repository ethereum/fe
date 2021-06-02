//! Fe Lowering.

use crate::types::FeModuleAst;
use fe_analyzer::context::Context;

mod mappers;
mod names;
mod utils;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower(context: &mut Context, module: FeModuleAst) -> FeModuleAst {
    mappers::module::module(context, module)
}
