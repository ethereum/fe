use fe_analyzer::namespace::types::AbiDecodeLocation;
use yultsur::*;

pub mod abi;
pub mod calls;
pub mod data;
pub mod structs;

/// Returns all functions that should be available during runtime.
pub fn std() -> Vec<yul::Statement> {
    vec![
        data::avail(),
        data::alloc(),
        data::alloc_mstoren(),
        data::free(),
        data::ccopym(),
        data::load_data_string(),
        data::mcopys(),
        data::scopym(),
        data::mcopym(),
        data::scopys(),
        data::mloadn(),
        data::sloadn(),
        data::cloadn(),
        data::mstoren(),
        data::sstoren(),
        data::dualkeccak256(),
        data::ceil32(),
        data::ternary(),
        abi::unpack(),
        abi::pack(AbiDecodeLocation::Calldata),
        abi::pack(AbiDecodeLocation::Memory),
    ]
}
