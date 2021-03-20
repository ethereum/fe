//! Fe Lowering.

use crate::types::FeModuleAst;
use fe_analyzer::Context;

mod mappers;
mod names;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower(context: &Context, module: FeModuleAst) -> FeModuleAst {
    mappers::module::module(context, module)
}
