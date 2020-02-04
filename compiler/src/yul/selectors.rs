use crate::abi;
use crate::errors::CompileError;
use vyper_parser::ast as vyp;
use yultsur::yul;

pub fn switch<'a>(stmts: &'a Vec<&'a vyp::ContractStmt<'a>>) -> Result<yul::Switch, CompileError> {
    // TODO: Implement expression that takes first 4 bytes of call data.
    let expression = yul::Expression::Literal(yul::Literal {
        literal: String::from("420"),
        yultype: None,
    });
    let cases = stmts
        .iter()
        .map(|stmt| case(stmt))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(yul::Switch { expression, cases })
}

fn case<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<yul::Case, CompileError> {
    let selector = abi::func_select(stmt)?;

    Ok(yul::Case {
        literal: Some(yul::Literal {
            literal: hex::encode(selector),
            yultype: None,
        }),
        block: yul::Block {
            statements: Vec::new(),
        },
    })
}
