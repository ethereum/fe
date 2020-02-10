use crate::yul::base;
use yultsur::yul;

pub fn code() -> yul::Code {
    yul::Code {
        block: yul::Block {
            statements: vec![
                yul::Statement::VariableDeclaration(yul::VariableDeclaration {
                    identifiers: vec![base::untyped_identifier("size")],
                    expression: Some(yul::Expression::FunctionCall(yul::FunctionCall {
                        identifier: base::untyped_identifier("datasize"),
                        arguments: vec![base::untyped_identifier_expr("\"runtime\"")],
                    })),
                }),
                yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                    identifier: base::untyped_identifier("datacopy"),
                    arguments: vec![
                        base::untyped_literal_expr("0"),
                        yul::Expression::FunctionCall(yul::FunctionCall {
                            identifier: base::untyped_identifier("dataoffset"),
                            arguments: vec![base::untyped_literal_expr("\"runtime\"")],
                        }),
                        base::untyped_identifier_expr("size"),
                    ],
                })),
                yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                    identifier: base::untyped_identifier("return"),
                    arguments: vec![
                        base::untyped_literal_expr("0"),
                        base::untyped_identifier_expr("size"),
                    ],
                })),
            ],
        },
    }
}

#[test]
fn test_constructor() {
    assert_eq!(
        code().to_string(),
        "code { let size := datasize(\"runtime\") datacopy(0, dataoffset(\"runtime\"), size) return(0, size) }",
        "incorrect constructor"
    )
}
