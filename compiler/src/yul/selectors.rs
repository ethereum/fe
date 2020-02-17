use crate::errors::CompileError;
use crate::yul::base;
use yultsur::yul;

/// Builds a switch statement from the contract ABI.
/// The switch's expression is the 4 left-most bytes in the calldata and each case is
/// defined as the keccak value of each function's signature (without return data).
pub fn switch(functions: &Vec<ethabi::Function>) -> Result<yul::Switch, CompileError> {
    let cases = functions
        .iter()
        .map(|function| case(function))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(yul::Switch {
        expression: expression(),
        cases,
    })
}

/// Builds an expression that loads the first 4 bytes of the calldata.
fn expression() -> yul::Expression {
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
fn case(function: &ethabi::Function) -> Result<yul::Case, CompileError> {
    let selector = vec![0]; // TODO: Load actual selector with ABI module.

    Ok(yul::Case {
        literal: Some(base::untyped_literal(&format!("0x{}", hex::encode(selector)))),
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

#[test]
fn test_expression() {
    assert_eq!(
        expression().to_string(),
        "shr(224, calldataload(0))",
        "Switch expression not correct."
    )
}
