use yultsur::*;

/// Return all contacts runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![create2(), create()]
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
