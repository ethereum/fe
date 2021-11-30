use crate::constants::{ERROR_FAILED_SEND_VALUE, ERROR_INSUFFICIENT_FUNDS_TO_SEND_VALUE};
use crate::operations::revert as revert_operations;
use yultsur::*;

/// Return all contacts runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![create2(), create(), send_value()]
}

/// Function that executes the `create2` operation.
pub fn create2() -> yul::Statement {
    function_definition! {
        function contract_create2(data_ptr, data_size, value, salt) -> return_address {
            (let mptr := alloc(data_size))
            (datacopy(mptr, data_ptr, data_size))
            (return_address := create2(value, mptr, data_size, salt))
        }
    }
}

/// Function that executes the `create` operation.
pub fn create() -> yul::Statement {
    function_definition! {
        function contract_create(data_ptr, data_size, value) -> return_address {
            (let mptr := alloc(data_size))
            (datacopy(mptr, data_ptr, data_size))
            (return_address := create(value, mptr, data_size))
        }
    }
}

/// Function that sends wei from the contract to another address
pub fn send_value() -> yul::Statement {
    function_definition! {
        // Inspired by: https://github.com/OpenZeppelin/openzeppelin-contracts/blob/5b28259dacf47fc208e03611eb3ba8eeaed63cc0/contracts/utils/Address.sol#L54-L59
        function send_value(to_address, value_in_wei) -> result {
            (if (lt((selfbalance()), value_in_wei)) { [revert_operations::error_revert_numeric(ERROR_INSUFFICIENT_FUNDS_TO_SEND_VALUE)] })
            (let success := call((gas()) , to_address, value_in_wei, 0, 0, 0, 0))
            (if (iszero(success)) { [revert_operations::error_revert_numeric(ERROR_FAILED_SEND_VALUE)] })
            // send_value returns the unit type but we need to return something.
            (result := 0x0)
        }
    }
}
