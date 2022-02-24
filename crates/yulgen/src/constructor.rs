use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::runtime::functions;
use crate::types::{AbiDecodeLocation, AbiType};
use yultsur::*;

/// Builds a constructor for a contract with no init function.
///
/// The contract is simply deployed by loading the code into memory and
/// returning it.
pub fn build() -> yul::Code {
    // we get the deployment statements and wrap them in a code block
    let deployment = deployment();
    code! { [deployment...] }
}

/// Builds a constructor for a contract with an init function.
pub fn build_with_init(
    contract_name: &str,
    init_function_name: &str,
    init_params: &[AbiType],
    init_callgraph: Vec<yul::Statement>,
    expects_ctx: bool,
) -> yul::Code {
    // Generate names for our constructor parameters.
    let (param_idents, param_exprs) = abi_names::vals("init", init_params.len());

    let decode_fns = functions::abi::decode_functions(init_params, AbiDecodeLocation::Memory);

    // Decode the parameters, if any are given.
    let maybe_decode_params = if init_params.is_empty() {
        statements! {}
    } else {
        let decode_expr = abi_operations::decode_data(
            init_params,
            expression! { params_start_mem },
            expression! { params_end_mem },
            AbiDecodeLocation::Memory,
        );

        statements! { (let [param_idents...] := [decode_expr]) }
    };

    // init function name after it is mapped.
    let init_function_name = identifier! { (init_function_name) };

    let contract_name = literal_expression! { (format!("\"{}\"", contract_name)) };

    let deployment = deployment();

    let init_call = if expects_ctx {
        // we pass in a `0` for the expected `Context` argument
        expression! { [init_function_name](0, [param_exprs...]) }
    } else {
        expression! { [init_function_name]([param_exprs...]) }
    };

    // Build a constructor that runs a user defined init function. Parameters for
    // init functions are appended to the end of the initialization code.
    //
    // The start of the init parameters in code is stored in `params_start_code`.
    // The parameters are immediately copied from this location into memory at
    // `mem_start`. From there, parameters are decoded and passed into the
    // init function.
    code! {
        // add init function and dependencies to scope
        [init_callgraph...]
        [decode_fns...]

        // copy the encoded parameters to memory
        (let params_start_code := datasize([contract_name]))
        (let params_end_code := codesize())
        (let params_size := sub(params_end_code, params_start_code))
        (let params_start_mem := alloc(params_size))
        (let params_end_mem := add(params_start_mem, params_size))
        (codecopy(params_start_mem, params_start_code, params_size))

        // decode the parameters from memory
        [maybe_decode_params...]

        // call the init function defined above
        (pop([init_call]))

        // deploy the contract
        [deployment...]
    }
}

/// Copies contract data to memory and returns it.
fn deployment() -> Vec<yul::Statement> {
    statements! {
        (let size := datasize("runtime"))
        (datacopy(0, (dataoffset("runtime")), size))
        (return(0, size))
    }
}
