use fe_analyzer::Context;

use crate::lowering::mappers::contracts;
use fe_parser::ast as fe;

/// Lowers a module.
pub fn module(context: &Context, module: fe::Module) -> fe::Module {
    let lowered_body = module
        .body
        .into_iter()
        .map(|stmt| match &stmt.kind {
            fe::ModuleStmt::TypeDef { .. } => stmt,
            fe::ModuleStmt::StructDef { .. } => stmt,
            fe::ModuleStmt::FromImport { .. } => stmt,
            fe::ModuleStmt::SimpleImport { .. } => stmt,
            fe::ModuleStmt::ContractDef { .. } => contracts::contract_def(context, stmt),
        })
        .collect();

    fe::Module { body: lowered_body }
}
