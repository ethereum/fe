use yultsur::yul;

/// Generates a Yul function that can be used to read mapped values for key value pairs of
/// any type within storage.
///
/// Yul does not natively support Maps, so we need to implement it ourselves. The basic design
/// is as follows:
///
/// Maps are represented as 256 bit uints during runtime. During compilation, we simply
/// replace all references to a map with the same unsigned integer. We use this value as
/// the prefix to our key values, which is then hashed and used to lookup the value in storage.
///
/// Storing values:
/// `storage[keccak(<map's u256 value>, key)] = value`
///
/// Reading values:
/// `storage[keccak(<map's u256 value>, key)]`
///
/// This function should return something along the lines of this:
/// ``` Yul
/// function _map_sload_u256_u256(m:u256, k:u256) -> v:u256 {
///     let p:u256 := usize()
///     mstore(msize(), m)
///     mstore(msize(), k)
///     v := sload(keccak(p, 2:u256))
/// }
/// ```
pub fn map_sload(key_type: yul::Type, value_type: yul::Type) -> yul::FunctionDefinition {
    yul::FunctionDefinition {
        name: yul::Identifier {
            // TODO: Format name with actual types
            identifier: String::from("_map_sload_u256_u256"),
            yultype: None
        },
        parameters: vec![
            yul::Identifier {
                identifier: String::from("m"),
                yultype: Some(yul::Type::Uint256)
            },
            yul::Identifier {
                identifier: String::from("k"),
                yultype: Some(key_type)
            },
        ],
        returns: vec![
            yul::Identifier {
                identifier: String::from("v"),
                yultype: Some(value_type)
            }
        ],
        block: yul::Block {
            statements: vec![
                yul::Statement::VariableDeclaration(yul::VariableDeclaration {
                    identifiers: vec![
                        yul::Identifier {
                            identifier: String::from("p"),
                            yultype: Some(yul::Type::Uint256)
                        }
                    ],
                    expression: Some(yul::Expression::FunctionCall(msize()))
                }),
                yul::Statement::Expression(yul::Expression::FunctionCall(
                    mstore_top(yul::Expression::Identifier(yul::Identifier {
                        identifier: String::from("m"),
                        yultype: None
                    }))
                )),
                yul::Statement::Expression(yul::Expression::FunctionCall(
                    mstore_top(yul::Expression::Identifier(yul::Identifier {
                        identifier: String::from("k"),
                        yultype: None
                    })
                ))),
                yul::Statement::Assignment(yul::Assignment {
                    identifiers: vec![yul::Identifier {
                        identifier: String::from("v"),
                        yultype: None
                    }],
                    expression: yul::Expression::FunctionCall(yul::FunctionCall {
                        identifier: yul::Identifier {
                            identifier: String::from("sload"),
                            yultype: None
                        },
                        arguments: vec![yul::Expression::FunctionCall(yul::FunctionCall {
                            identifier: yul::Identifier {
                                identifier: String::from("keccak"),
                                yultype: None
                            },
                            arguments: vec![
                                yul::Expression::Identifier(yul::Identifier {
                                    identifier: String::from("p"),
                                    yultype: None
                                }),
                                yul::Expression::Identifier(yul::Identifier {
                                    identifier: String::from("2"),
                                    yultype: Some(yul::Type::Uint256)
                                })
                            ]
                        })]
                    })
                })
            ]
        }
    }
}

/// Produces a Yul function for storing map values.
///
/// It should return something similar to this:
///
/// ``` Yul
/// function _map_sstore_u256_u256(m:u256, k:u256, v:u256) {
///     let p:u256 := usize()
///     mstore(msize(), m)
///     mstore(msize(), k)
///     sstore(keccak(p, 2:u256), v)
/// }
/// ```
pub fn map_sstore(key_type: yul::Type, value_type: yul::Type) -> yul::FunctionDefinition {
    yul::FunctionDefinition {
        name: yul::Identifier {
            // TODO: Format name with actual types
            identifier: String::from("_map_sstore_u256_u256"),
            yultype: None
        },
        parameters: vec![
            yul::Identifier {
                identifier: String::from("m"),
                yultype: Some(yul::Type::Uint256)
            },
            yul::Identifier {
                identifier: String::from("k"),
                yultype: Some(key_type)
            },
            yul::Identifier {
                identifier: String::from("v"),
                yultype: Some(value_type)
            }
        ],
        returns: Vec::new(),
        block: yul::Block {
            statements: vec![
                yul::Statement::VariableDeclaration(yul::VariableDeclaration {
                    identifiers: vec![
                        yul::Identifier {
                            identifier: String::from("p"),
                            yultype: Some(yul::Type::Uint256)
                        }
                    ],
                    expression: Some(yul::Expression::FunctionCall(msize()))
                }),
                yul::Statement::Expression(yul::Expression::FunctionCall(
                    mstore_top(yul::Expression::Identifier(yul::Identifier {
                        identifier: String::from("m"),
                        yultype: None
                    }))
                )),
                yul::Statement::Expression(yul::Expression::FunctionCall(
                    mstore_top(yul::Expression::Identifier(yul::Identifier {
                        identifier: String::from("k"),
                        yultype: None
                    })
                    ))),
                yul::Statement::Expression(yul::Expression::FunctionCall(yul::FunctionCall {
                    identifier: yul::Identifier {
                        identifier: String::from("sstore"),
                        yultype: None
                    },
                    arguments: vec![
                        yul::Expression::FunctionCall(yul::FunctionCall {
                            identifier: yul::Identifier {
                                identifier: String::from("keccak"),
                                yultype: None
                            },
                            arguments: vec![
                                yul::Expression::Identifier(yul::Identifier {
                                    identifier: String::from("p"),
                                    yultype: None
                                }),
                                yul::Expression::Identifier(yul::Identifier {
                                    identifier: String::from("2"),
                                    yultype: Some(yul::Type::Uint256)
                                })
                            ]
                        }),
                        yul::Expression::Identifier(yul::Identifier {
                            identifier: String::from("v"),
                            yultype: None
                        })
                    ]
                }))
            ]
        }
    }
}

fn mstore_top(val: yul::Expression) -> yul::FunctionCall {
    yul::FunctionCall {
        identifier: yul::Identifier {
            identifier: String::from("mstore"),
            yultype: None
        },
        arguments: vec![
            yul::Expression::FunctionCall(msize()),
            val
        ]
    }
}

fn msize() -> yul::FunctionCall {
    yul::FunctionCall {
        identifier: yul::Identifier {
            identifier: String::from("msize"),
            yultype: None
        },
        arguments: Vec::new()
    }
}

#[test]
fn test_map_sload() {
    let func = map_sload(yul::Type::Uint256, yul::Type::Uint256);

    assert_eq!(
        func.to_string(),
        "function _map_sload_u256_u256(m:u256, k:u256) -> v:u256 { let p:u256 := msize() mstore(msize(), m) mstore(msize(), k) v := sload(keccak(p, 2:u256)) }",
        "Functions do not match."
    )
}
#[test]
fn test_map_sstore() {
    let func = map_sstore(yul::Type::Uint256, yul::Type::Uint256);

    assert_eq!(
        func.to_string(),
        "function _map_sstore_u256_u256(m:u256, k:u256, v:u256) { let p:u256 := msize() mstore(msize(), m) mstore(msize(), k) sstore(keccak(p, 2:u256), v) }",
        "Functions do not match."
    )
}
