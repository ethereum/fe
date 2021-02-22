use fe_analyzer::namespace::types::AbiDecodeLocation;
use yultsur::*;

pub mod abi;
pub mod contracts;
pub mod data;
pub mod math;
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
        data::bytes_sloadn(),
        data::cloadn(),
        data::mstoren(),
        data::sstoren(),
        data::bytes_sstoren(),
        data::map_value_ptr(),
        data::ceil32(),
        data::ternary(),
        data::set_zero(),
        abi::unpack(),
        abi::pack(AbiDecodeLocation::Calldata),
        abi::pack(AbiDecodeLocation::Memory),
        contracts::create2(),
        contracts::create(),
        math::checked_add_u256(),
        math::checked_add_u128(),
        math::checked_add_u64(),
        math::checked_add_u32(),
        math::checked_add_u16(),
        math::checked_add_u8(),
        math::checked_add_i256(),
        math::checked_add_i128(),
        math::checked_add_i64(),
        math::checked_add_i32(),
        math::checked_add_i16(),
        math::checked_add_i8(),
    ]
}
