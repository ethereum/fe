use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::types::{to_abi_types, AbiDecodeLocation};
use fe_analyzer::namespace::types::FixedSize;
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
///
/// We include the entire contact runtime inside of the constructor (without the
/// ABI dispatcher), run the init function, and return the contract code.
pub fn build_with_init(
    contract_name: &str,
    init_func: yul::Statement,
    init_params: Vec<FixedSize>,
    runtime: Vec<yul::Statement>,
) -> yul::Code {
    // Generate names for our constructor parameters.
    let (param_idents, param_exprs) = abi_names::vals("init", init_params.len());

    // Decode the parameters, if any are given.
    let maybe_decode_params = if init_params.is_empty() {
        statements! {}
    } else {
        let decode_expr = abi_operations::decode_data(
            &to_abi_types(&init_params),
            expression! { params_start_mem },
            expression! { params_end_mem },
            AbiDecodeLocation::Memory,
        );

        statements! { (let [param_idents...] := [decode_expr]) }
    };

    // init function name after it is mapped.
    let init_func_name = identifier! { ("$$__init__") };

    let contract_name = literal_expression! { (format!("\"{}\"", contract_name)) };

    let deployment = deployment();

    // Build a constructor that runs a user defined init function. Parameters for
    // init functions are appended to the end of the initialization code.
    //
    // The start of the init parameters in code is stored in `params_start_code`.
    // The parameters are immediately copied from this location into memory at
    // `mem_start`. From there, parameters are decoded and passed into the
    // init function.
    code! {
        // add init function to the scope
        [init_func]

        // add the entire contract runtime
        [runtime...]

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
        (pop(([init_func_name]([param_exprs...]))))

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
