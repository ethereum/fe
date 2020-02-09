use crate::abi;
use crate::errors::CompileError;
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
                identifier: yul::Identifier {
                    identifier: String::from("calldataload"),
                    yultype: None,
                },
                arguments: vec![yul::Expression::Literal(yul::Literal {
                    literal: String::from("0"),
                    yultype: None,
                })],
            }),
            yul::Expression::Literal(yul::Literal {
                literal: String::from("224"),
                yultype: None,
            }),
        ],
    })
}

fn case<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<yul::Case, CompileError> {
    let selector = abi::func_select(stmt)?;

    Ok(yul::Case {
        literal: Some(yul::Literal {
            literal: format!("0x{}", hex::encode(selector)),
            yultype: None, // TODO: Uncomment once solc supports types: Some(yul::Type::Uint32),
        }),
        block: yul::Block {
            statements: Vec::new(),
        },
    })
}

#[test]
fn test_expression() {
    assert_eq!(
        expression().to_string(),
        "shr(calldataload(0), 224)",
        "Switch expression not correct."
    )
}
