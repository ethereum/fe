use crate::errors::CompileError;
use crate::yul::base;
use yultsur::yul;
use tiny_keccak::{Hasher, Keccak};

/// Builds a switch statement from the contract ABI.
/// The switch's expression is the 4 left-most bytes in the calldata and each case is
/// defined as the keccak value of each function's signature (without return data).
pub fn switch(functions: Vec<&ethabi::Function>) -> Result<yul::Switch, CompileError> {
    let cases = functions
        .into_iter()
        .map(|function| case(function))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(yul::Switch {
        expression: expression(),
        cases,
    })
}

/// Builds an expression that loads the first 4 bytes of the calldata.
pub fn expression() -> yul::Expression {
    yul::Expression::FunctionCall(yul::FunctionCall {
        identifier: base::untyped_identifier("shr"),
        arguments: vec![
            base::untyped_literal_expr("224"),
            yul::Expression::FunctionCall(yul::FunctionCall {
                identifier: base::untyped_identifier("calldataload"),
                arguments: vec![base::untyped_literal_expr("0")],
            }),
        ],
    })
}

/// Builds a switch case from the function. It matches the selector and calls
/// the function as described in the ABI. The value (if any) returned by the
/// function is stored in memory and returned by the contract.
///
/// Currently, this assumes each input and the single output is 256 bits.
/// TODO: Handle types of different sizes: https://solidity.readthedocs.io/en/v0.6.2/abi-spec.html#types
pub fn case(function: &ethabi::Function) -> Result<yul::Case, CompileError> {
    let selector = Some(selector_literal(function.signature()));

    if function.outputs.is_empty() {
        return Ok(
            yul::Case {
                literal: selector,
                block: yul::Block {
                    statements: vec![
                        yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                            identifier: base::untyped_identifier("foo"),
                            arguments: vec![]
                        }))
                    ]
                }
            }
        )
    }

    Ok(yul::Case {
        literal: selector,
        block: yul::Block {
            statements: vec![
                yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                    identifier: base::untyped_identifier("mstore"),
                    arguments: vec![
                        base::untyped_literal_expr("0"),
                        yul::Expression::FunctionCall(yul::FunctionCall {
                            identifier: base::untyped_identifier(&function.name),
                            arguments: (0..function.inputs.len())
                                .map(|n| {
                                    base::untyped_literal_expr(
                                        format!("calldataload({})", n * 32 + 4).as_ref(),
                                    )
                                })
                                .collect(),
                        }),
                    ],
                })),
                yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                    identifier: base::untyped_identifier("return"),
                    arguments: vec![
                        base::untyped_literal_expr("0"),
                        base::untyped_literal_expr("32"),
                    ],
                })),
            ],
        },
    })
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

#[cfg(test)]
mod tests {
    use crate::yul::selectors::{expression, selector_literal, case, switch};
    use stringreader::StringReader;

    #[test]
    fn test_expression() {
        assert_eq!(
            expression().to_string(),
            "shr(224, calldataload(0))",
            "Switch expression not correct."
        )
    }

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
            "Incorrect selector"
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
            "Incorrect case"
        );
    }

    #[test]
    fn test_switch_basic() {
        let json_abi = r#"[{"name": "foo", "type": "function", "inputs": [], "outputs": []}]"#;
        let abi = ethabi::Contract::load(StringReader::new(json_abi)).expect("Unable to load abi.");
        let functions = abi.functions().collect();

        assert_eq!(
            switch(functions).expect("Unable to build selector").to_string(),
            String::from("switch shr(224, calldataload(0)) case 0xc2985578 { foo() } "),
            "Incorrect selector"
        )
    }
}