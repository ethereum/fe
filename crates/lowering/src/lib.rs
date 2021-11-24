//! Fe Lowering.

use fe_analyzer::namespace::items::{IngotId, ModuleId};

mod ast_utils;
mod context;
pub mod db;
mod mappers;
mod names;
mod utils;

pub use db::{LoweringDb, TestDb};

/// Lower a Fe module
///
/// Interns a module with the lowered AST and returns its ID.
pub fn lower_module(db: &dyn LoweringDb, module_id: ModuleId) -> ModuleId {
    db.lowered_module(module_id)
}

/// Lower a Fe ingot
///
/// Interns an ingot with the lowered module ASTs and returns its ID.
pub fn lower_ingot(db: &dyn LoweringDb, ingot_id: IngotId) -> IngotId {
    db.lowered_ingot(ingot_id)
}
