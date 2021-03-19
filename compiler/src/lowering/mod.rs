//! Fe Lowering.

use crate::types::FeModuleAst;
use fe_analyzer::Context;

mod mappers;
mod names;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower<'a>(_context: &Context, module: &FeModuleAst<'a>) -> FeModuleAst<'a> {
    module.clone()
}
