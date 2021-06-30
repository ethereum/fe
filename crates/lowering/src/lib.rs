//! Fe Lowering.

use fe_analyzer::context::Context as AnalyzerContext;
use fe_parser::ast;

mod context;
mod mappers;
mod names;
mod utils;

/// Lowers the Fe source AST to a Fe HIR AST.
pub fn lower(analysis: &AnalyzerContext, module: ast::Module) -> ast::Module {
    mappers::module::module(analysis, module)
}
