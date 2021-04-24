use crate::yul::operations::abi as abi_operations;
use fe_analyzer::namespace::types::{AbiDecodeLocation, FixedSize};
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
    // get the deplyment statements
    let deployment = deployment();
    // we need to decode the init parameters before passing them into `__init__`
    // `params_start_mem` is added to the scope of the code block found below
    let decoded_params = abi_operations::decode(
        init_params,
        expression! { params_start_mem },
        AbiDecodeLocation::Memory,
    );
    // the name of the user defined init function after it is mapped
    let init_func_name = identifier! { ("$$__init__") };
    let contract_name = literal_expression! { (format!("\"{}\"", contract_name)) };

    // Build a constructor that runs a user defined init function. Parameters for
    // init functions are appended to the end of the initialization code.
    //
    // The start of the init parameters in code is stored in `params_start_code`.
    // The parameters are immediately copied from this location into memory at
    // `mem_start`. From there, parameters are decoded and passed into the
    // init function.
    code! {
        // copy params to memory where they can be decoded
        (let params_start_code := datasize([contract_name]))
        (let params_end_code := codesize())
        (let params_size := sub(params_end_code, params_start_code))
        (let params_start_mem := alloc(params_size))
        (codecopy(params_start_mem, params_start_code, params_size))

        // add init function amd call it
        [init_func]
        ([init_func_name]([decoded_params...]))

        // add the runtime functions
        [runtime...]

        // deploy the contract
        [deployment...]
    }
}

fn deployment() -> Vec<yul::Statement> {
    statements! {
        (let size := datasize("runtime"))
        (datacopy(0, (dataoffset("runtime")), size))
        (return(0, size))
    }
}

#[test]
fn test_constructor_without_func() {
    assert_eq!(
        build().to_string(),
        r#"code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) }"#,
    )
}
