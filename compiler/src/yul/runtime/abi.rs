use crate::errors::CompileError;
use crate::yul::base;
use tiny_keccak::{Hasher, Keccak};
use yultsur::yul;
use yultsur::*;

struct SimpParam {
    size: usize,
    is_ptr: bool,
}

impl From<ethabi::Param> for SimpParam {
    fn from(param: ethabi::Param) -> Self {
        let (size, is_ptr) = match param.kind {
            ethabi::ParamType::FixedArray(_, dim) => (dim * 32, true),
            _ => (32, false),
        };

        SimpParam {
            size,
            is_ptr,
        }
    }
}

/// Builds a switch statement from the contract ABI.
/// The switch's expression is the 4 left-most bytes in the calldata and each case is
/// defined as the keccak value of each function's signature (without return data).
pub fn switch(functions: Vec<&ethabi::Function>) -> Result<yul::Statement, CompileError> {
    let cases = functions
        .into_iter()
        .map(|function| case(function))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (callval(0, 4))
        [cases...]
    })
}

/// Builds a switch case from the function. It matches the selector and calls
/// the function as described in the ABI. The value (if any) returned by the
/// function is stored in memory and returned by the contract.
///
/// Currently, this assumes each input and the single output is 256 bits.
/// TODO: Handle types of different sizes: https://solidity.readthedocs.io/en/v0.6.2/abi-spec.html#types
pub fn case(function: &ethabi::Function) -> Result<yul::Case, CompileError> {
    let selector = selector_literal(function.signature());
    let name = base::untyped_identifier(&function.name);

    let param = SimpParam::from(function.outputs.first().unwrap().clone());
    let return_size = base::untyped_literal_expr(&param.size.to_string());

    if param.is_ptr {
        Ok(case! {
            case [selector] {
                (let return_ptr := [name]((callval(4, 32))))
                (return(return_ptr, [return_size]))
            }
        })
    } else {
        Ok(case! {
            case [selector] {
                (let return_val := [name]((callval(4, 32))))
                (mstore(0, return_val))
                (return(0, 32))
            }
        })
    }
}

/// Computes the keccak-256 value of the input portion of the function signature and returns the
/// first 4 bytes.
///
/// Example: "foo(uint256):(uint256)" => keccak256("foo(uint256)")
pub fn selector_literal(sig: String) -> yul::Literal {
    let mut sig_halves = sig.split(":");

    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 4];

    if let Some(first_half) = sig_halves.next() {
        keccak.update(first_half.as_bytes());
        keccak.finalize(&mut selector);
    }

    base::untyped_literal(&format!("0x{}", hex::encode(selector)))
}

/*
#[cfg(test)]
mod tests {
    use crate::yul::runtime::selectors::{case, expression, selector_literal, switch};
    use stringreader::StringReader;

    #[test]
    fn test_selector_literal_basic() {
        let json_abi = r#"[{"name": "foo", "type": "function", "inputs": [], "outputs": []}]"#;
        let abi = ethabi::Contract::load(StringReader::new(json_abi)).expect("Unable to load abi.");
        let ref foo = abi.functions["foo"][0];

        assert_eq!(
            selector_literal(foo.signature()).to_string(),
            String::from("0xc2985578"),
            "Incorrect selector"
        )
    }

    #[test]
    fn test_selector_literal() {
        let json_abi = r#"[{"name": "foo", "type": "function", "inputs": [{ "name": "bar", "type": "uint256" }], "outputs": []}]"#;
        let abi = ethabi::Contract::load(StringReader::new(json_abi)).expect("Unable to load abi.");
        let ref foo = abi.functions["foo"][0];

        assert_eq!(
            selector_literal(foo.signature()).to_string(),
            String::from("0x2fbebd38"),
        )
    }

    #[test]
    fn test_case() {
        let json_abi = r#"[{"name": "foo", "type": "function", "inputs": [{ "name": "bar", "type": "uint256" }], "outputs": [{ "name": "baz", "type": "uint256" }]}]"#;
        let abi = ethabi::Contract::load(StringReader::new(json_abi)).expect("Unable to load abi.");
        let ref foo = abi.functions["foo"][0];

        assert_eq!(
            case(foo).expect("Unable to build case.").to_string(),
            String::from("case 0x2fbebd38 { mstore(0, foo(calldataload(4))) return(0, 32) }"),
        );
    }

    #[test]
    fn test_switch_basic() {
        let json_abi = r#"[{"name": "foo", "type": "function", "inputs": [], "outputs": []}]"#;
        let abi = ethabi::Contract::load(StringReader::new(json_abi)).expect("Unable to load abi.");
        let functions = abi.functions().collect();

        assert_eq!(
            switch(functions)
                .expect("Unable to build selector")
                .to_string(),
            String::from("switch callval(0, 4) case 0xc2985578 { foo() } "),
        )
    }
}
*/
