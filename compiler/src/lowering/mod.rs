//! Fe Lowering.

use crate::types::FeModuleAst;
use fe_analyzer::context::Context as AnalyzerContext;

mod context;
mod mappers;
mod names;
mod utils;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower(analysis: &AnalyzerContext, module: FeModuleAst) -> FeModuleAst {
    mappers::module::module(analysis, module)
}
