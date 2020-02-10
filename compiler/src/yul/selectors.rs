use crate::abi;
use crate::errors::CompileError;
use crate::yul::base;
use vyper_parser::ast as vyp;
use yultsur::yul;

pub fn switch<'a>(stmts: &'a Vec<&'a vyp::ContractStmt<'a>>) -> Result<yul::Switch, CompileError> {
    let cases = stmts
        .iter()
        .map(|stmt| case(stmt))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(yul::Switch {
        expression: expression(),
        cases,
    })
}

fn expression() -> yul::Expression {
    yul::Expression::FunctionCall(yul::FunctionCall {
        identifier: yul::Identifier {
            identifier: String::from("shr"),
            yultype: None,
        },
        arguments: vec![
            yul::Expression::FunctionCall(yul::FunctionCall {
                identifier: base::untyped_identifier("calldataload"),
                arguments: vec![base::untyped_literal_expr("0")],
            }),
            base::untyped_literal_expr("224"),
        ],
    })
}

fn case<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<yul::Case, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual,
        name,
        args,
        return_type,
        body,
    } = stmt
    {
        let selector = abi::func_select(stmt)?;

        return Ok(yul::Case {
            literal: Some(yul::Literal {
                literal: format!("0x{}", hex::encode(selector)),
                yultype: None, // TODO: Uncomment once solc supports types: Some(yul::Type::Uint32),
            }),
            block: yul::Block {
                statements: vec![
                    yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                        identifier: base::untyped_identifier("mstore"),
                        arguments: vec![
                            base::untyped_literal_expr("0"),
                            yul::Expression::FunctionCall(yul::FunctionCall {
                                identifier: base::untyped_identifier(name.node),
                                arguments: (4..4 + args.len())
                                    .map(|n| {
                                        base::untyped_literal_expr(
                                            format!("calldataload({})", n).as_ref(),
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
                            base::untyped_literal_expr("1"),
                        ],
                    })),
                ],
            },
        });
    }

    return Err(CompileError::static_str("Requires FuncDef"));
}

#[test]
fn test_expression() {
    assert_eq!(
        expression().to_string(),
        "shr(calldataload(0), 224)",
        "Switch expression not correct."
    )
}
