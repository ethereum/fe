use yultsur::*;

pub mod abi;
pub mod contracts;
pub mod data;
pub mod math;
pub mod revert;

/// Returns all functions that should be available during runtime.
pub fn std() -> Vec<yul::Statement> {
    [
        contracts::all(),
        abi::all(),
        data::all(),
        math::all(),
        revert::all(),
    ]
    .concat()
}
