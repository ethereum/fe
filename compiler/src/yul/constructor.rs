use crate::yul::{
    operations,
    runtime,
};
use fe_semantics::namespace::types::{
    AbiEncoding,
    FixedSize,
};
use yultsur::*;

/// Builds a contract constructor.
///
/// Takes an optional init function and its parameter types.
pub fn build(init: Option<(yul::Statement, Vec<FixedSize>)>) -> yul::Code {
    // statements that return the contract code
    let deploy_stmts = statements! {
        (let size := datasize("runtime"))
        (datacopy(0, (dataoffset("runtime")), size))
        (return(0, size))
    };

    let block = if let Some((init, params)) = init {
        // build a constructor with an init function

        // map the init params to decode expressions
        let mut offset = 0;
        let mut decoded_params = vec![];
        for param in params.iter() {
            let offset_expr = literal_expression! { (offset) };
            decoded_params.push(operations::decode_mem(
                param.to_owned(),
                expression! { add(mem_start, [offset_expr]) },
            ));
            offset += param.abi_size();
        }

        // The final value of `offset` is equal to the total size of the ABI encoded
        // parameters. We reuse this to copy the parameters from code to memory.
        let params_size = literal_expression! { (offset) };

        // the standard runtime functions need to be made available in the constructor
        let runtime_functions = runtime::functions::std();

        // Build a constructor that runs a user defined init function. Parameters for
        // init functions are appended to the end of the initialization code.
        //
        // The start of the init parameters in code is stored in `code_start`. The
        // parameters are immediately copied from this location into memory at
        // `mem_start`. From there, parameters are decoded and passed into the
        // init function.
        block! {
            (let code_start := datasize("Contract"))
            (let mem_start := alloc([params_size.clone()]))
            (codecopy(mem_start, code_start, [params_size]))
            [init]
            (__init__([decoded_params...]))
            [runtime_functions...]
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
        build(None).to_string(),
        r#"code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) }"#,
    )
}
