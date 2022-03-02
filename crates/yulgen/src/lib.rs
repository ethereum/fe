//! Fe to Yul compiler.

pub use crate::db::{Db, YulgenDb};
use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::AnalyzerDb;
use indexmap::map::IndexMap;

pub mod constants;
pub mod constructor;
mod context;
mod db;
mod mappers;
pub mod names;
pub mod operations;
pub mod runtime;
pub mod types;
mod utils;

/// Compiles a lowered Fe module to Yul contracts.
///
/// Returns a `contract_name -> hex_encoded_bytecode` map.
///
/// # Panics
///
/// Any failure to compile an AST to Yul is considered a bug, and thus panics.
/// Invalid ASTs should be caught by an analysis step prior to Yul generation.
pub fn compile(db: &dyn YulgenDb, module: ModuleId) -> IndexMap<String, String> {
    db.compile_module(module)
}

pub fn compile_runtimes(db: &dyn YulgenDb, module: ModuleId) -> IndexMap<String, String> {
    db.compile_module_runtimes(module)
}
