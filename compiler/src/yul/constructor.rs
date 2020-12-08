use crate::yul::abi::operations as abi_operations;
use fe_semantics::namespace::types::{
    AbiDecodeLocation,
    FixedSize,
};
use yultsur::*;

/// Builds a contract constructor.
///
/// Takes an optional init function and its parameter types.
pub fn build(
    init: Option<(yul::Statement, Vec<FixedSize>)>,
    runtime: Vec<yul::Statement>,
) -> yul::Code {
    // statements that return the contract code
    let deploy_stmts = statements! {
        (let size := datasize("runtime"))
        (datacopy(0, (dataoffset("runtime")), size))
        (return(0, size))
    };

    let block = if let Some((init, params)) = init {
        // build a constructor with an init function

        // decode operations for `__init__` parameters
        let decoded_params = abi_operations::decode(
            params,
            expression! { params_start_mem },
            AbiDecodeLocation::Memory,
        );

        // Build a constructor that runs a user defined init function. Parameters for
        // init functions are appended to the end of the initialization code.
        //
        // The start of the init parameters in code is stored in `params_start_code`.
        // The parameters are immediately copied from this location into memory at
        // `mem_start`. From there, parameters are decoded and passed into the
        // init function.
        let init_func = identifier! { ("$$__init__") };
        block! {
            // copy params to memory where they can be decoded
            (let params_start_code := datasize("Contract"))
            (let params_end_code := codesize())
            (let params_size := sub(params_end_code, params_start_code))
            (let params_start_mem := alloc(params_size))
            (codecopy(params_start_mem, params_start_code, params_size))

            // add init function amd call it
            [init]
            ([init_func]([decoded_params...]))

            // add the runtime functions
            [runtime...]

            // deploy the contract
            [deploy_stmts...]
        }
    } else {
        // build a constructor without an init function
        block! { [deploy_stmts...] }
    };

    yul::Code { block }
}

#[test]
fn test_constructor_without_func() {
    assert_eq!(
        build(None, vec![]).to_string(),
        r#"code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) }"#,
    )
}
