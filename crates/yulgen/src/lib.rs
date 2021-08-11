//! Fe to Yul compiler.

use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::AnalyzerDb;
use std::collections::HashMap;
use yultsur::yul;

pub mod constants;
pub mod constructor;
mod context;
mod mappers;
pub mod names;
pub mod operations;
pub mod runtime;
pub mod types;
mod utils;

/// The name of a Fe contract.
pub type ContractName = String;
/// The intermediate representation of a contract as a string object.
pub type YulIr = String;
/// A mapping of contract names and their Yul IR.
pub type NamedYulContracts = HashMap<ContractName, YulIr>;

/// Compiles Fe source code to Yul.
///
/// # Panics
///
/// Any failure to compile an AST to Yul is considered a bug, and thus panics.
/// Invalid ASTs should be caught by an analysis step prior to Yul generation.
pub fn compile(db: &dyn AnalyzerDb, module: ModuleId) -> NamedYulContracts {
    mappers::module::module(db, module)
        .drain()
        .map(|(name, object)| (name, to_safe_json(object)))
        .collect::<NamedYulContracts>()
}

fn to_safe_json(obj: yul::Object) -> String {
    normalize_object(obj).to_string().replace("\"", "\\\"")
}

fn normalize_object(obj: yul::Object) -> yul::Object {
    let data = obj
        .data
        .into_iter()
        .map(|data| yul::Data {
            name: data.name,
            value: data
                .value
                .replace('\\', "\\\\\\\\")
                .replace('\n', "\\\\n")
                .replace("\"", "\\\\\"")
                .replace('\r', "\\\\r")
                .replace('\t', "\\\\t"),
        })
        .collect::<Vec<_>>();
    yul::Object {
        name: obj.name,
        code: obj.code,
        objects: obj
            .objects
            .into_iter()
            .map(normalize_object)
            .collect::<Vec<_>>(),
        data,
    }
}
