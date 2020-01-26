use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

struct CompileError;

type CompileResult<'a, T> = Result<Option<T>, CompileError>;
type SharedScope<'a> = Rc<RefCell<Scope<'a>>>;

struct Scope<'a> {
    parent: Option<SharedScope<'a>>,
    type_defs: HashMap<&'a str, &'a vyp::TypeDesc<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            parent: None,
            type_defs: HashMap::new(),
        }
    }

    fn extend(parent: SharedScope) -> SharedScope {
        Rc::new(RefCell::new(Scope {
            parent: Some(parent),
            type_defs: HashMap::new(),
        }))
    }

    fn top(mut current: SharedScope) -> SharedScope {
        while current.borrow().parent.is_some() {
            let parent = Rc::clone(current.borrow().parent.as_ref().unwrap());
            current = parent;
        }

        current
    }
}

fn module<'a>(module: &'a vyp::Module<'a>) -> CompileResult<'a, yul::Statement> {
    let mut scope = Rc::new(RefCell::new(Scope::new()));

    let statements_result: Result<Vec<Option<yul::Statement>>, CompileError> = module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect();

    let statements = statements_result?.into_iter().filter_map(|s| s).collect();

    Ok(Some(yul::Statement::Block(yul::Block { statements })))
}

fn module_stmt<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => type_def(scope, stmt),
        vyp::ModuleStmt::ContractDef { .. } => contract_def(scope, stmt),
        _ => Err(CompileError),
    }
}

fn contract_def<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    let new_scope = Scope::extend(scope);

    if let vyp::ModuleStmt::ContractDef { name, body } = stmt {
        let statements_result: Result<Vec<Option<yul::Statement>>, CompileError> = body
            .iter()
            .map(|stmt| contract_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect();

        let statements = statements_result?.into_iter().filter_map(|s| s).collect();

        return Ok(Some(yul::Statement::ContractDefinition(
            yul::ContractDefinition {
                name: yul::Identifier {
                    identifier: String::from(name.node),
                    yultype: None,
                },
                block: yul::Block { statements },
            },
        )));
    }

    Err(CompileError)
}

fn contract_stmt<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::ContractStmt::FuncDef { .. } => func_def(scope, stmt),
        _ => Err(CompileError),
    }
}

fn type_def<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        scope.borrow_mut().type_defs.insert(name.node, &typ.node);
        return Ok(None);
    }

    Err(CompileError)
}

fn func_def<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    if let vyp::ContractStmt::FuncDef {
        qual,
        name,
        args,
        return_type,
        body,
    } = stmt
    {
        let new_scope = Scope::extend(scope);

        let parameters_result: Result<Vec<Option<yul::Identifier>>, CompileError> = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&new_scope), &arg.node))
            .collect();

        let parameters = parameters_result?.into_iter().filter_map(|s| s).collect();

        let returns = if return_type.is_some() {
            vec![yul::Identifier {
                identifier: String::from("return_val"),
                yultype: None,
            }]
        } else {
            Vec::new()
        };

        let statements_result: Result<Vec<Option<yul::Statement>>, CompileError> = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect();

        let statements = statements_result?.into_iter().filter_map(|s| s).collect();

        return Ok(Some(yul::Statement::FunctionDefinition(
            yul::FunctionDefinition {
                name: yul::Identifier {
                    identifier: String::from(name.node),
                    yultype: None,
                },
                parameters,
                returns,
                block: yul::Block { statements },
            },
        )));
    }

    Err(CompileError)
}

fn func_def_arg<'a>(
    scope: SharedScope<'a>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> CompileResult<'a, yul::Identifier> {
    Ok(Some(yul::Identifier {
        identifier: String::from(arg.name.node),
        yultype: type_desc(scope, &arg.typ.node)?,
    }))
}

fn func_stmt<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        _ => Err(CompileError),
    }
}

fn func_return<'a>(
    scope: SharedScope<'a>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    if let vyp::FuncStmt::Return { value: Some(value) } = stmt {
        let return_value = expr(scope, &value.node)?;

        if let Some(return_value) = return_value {
            return Ok(Some(yul::Statement::Assignment(yul::Assignment {
                identifiers: vec![yul::Identifier {
                    identifier: String::from("return_val"),
                    yultype: None,
                }],
                expression: return_value,
            })));
        }
    }

    Err(CompileError)
}

fn type_desc<'a>(
    scope: SharedScope<'a>,
    desc: &'a vyp::TypeDesc<'a>,
) -> CompileResult<'a, yul::Type> {
    if let vyp::TypeDesc::Base { base } = desc {
        if let Some(custom_type) = Scope::top(Rc::clone(&scope)).borrow().type_defs.get(base) {
            return type_desc(scope, *custom_type);
        }
    }

    Ok(Some(match desc {
        vyp::TypeDesc::Base { base: "u256" } => yul::Type::Uint256,
        _ => return Err(CompileError),
    }))
}

fn expr<'a>(scope: SharedScope<'a>, expr: &'a vyp::Expr<'a>) -> CompileResult<'a, yul::Expression> {
    Ok(Some(match expr {
        vyp::Expr::Name(name) => yul::Expression::Identifier(yul::Identifier {
            identifier: String::from(*name),
            yultype: None,
        }),
        _ => { return Err(CompileError) },
    }))
}

#[cfg(test)]
mod tests {
    //use crate::yul::{contract_def, func_def, module, type_def, Scope};
    use crate::yul::{contract_def, func_def, module, type_def, Scope};
    use std::cell::RefCell;
    use std::rc::Rc;
    use vyper_parser::ast::TypeDesc;
    use vyper_parser::parsers;

    #[test]
    fn test_compile_type_def() {
        let toks = vyper_parser::get_parse_tokens("type Num = u256").unwrap();
        let stmt = parsers::type_def(&toks[..]).unwrap().1.node;

        let mut scope = Rc::new(RefCell::new(Scope::new()));

        // Expecting the type definition to exist within new_scope.
        // There is no resulting statement from compiling a TypeDef.
        type_def(Rc::clone(&scope), &stmt);

        assert_eq!(
            scope.borrow().type_defs["Num"],
            &TypeDesc::Base { base: "u256" },
            "Compilation of type definition failed."
        );
    }

    #[test]
    fn test_compile_func_def() {
        let toks =
            vyper_parser::get_parse_tokens("def double(x: u256) -> u256:\n   return x").unwrap();
        let stmt = parsers::func_def(&toks[..]).unwrap().1.node;

        let mut scope = Rc::new(RefCell::new(Scope::new()));

        if let Ok(Some(statement)) = func_def(Rc::clone(&scope), &stmt) {
            assert_eq!(
                statement.to_string(),
                "function double(x:u256) -> return_val { return_val := x }",
                "Compilation of function definition failed."
            );
        } else {
            assert!(false, "Unexpected error.");
        }
    }

    #[test]
    fn test_contract_def() {
        let vyp_code = "contract Foo:\
                      \n  pub def bar(x: u256) -> u256:\
                      \n    return x";
        let toks = vyper_parser::get_parse_tokens(vyp_code).unwrap();
        let stmt = parsers::contract_def(&toks[..]).unwrap().1.node;

        let mut scope = Rc::new(RefCell::new(Scope::new()));

        if let Ok(Some(statement)) = contract_def(scope, &stmt) {
            assert_eq!(
                statement.to_string(),
                "object \"Foo\" { function bar(x:u256) -> return_val { return_val := x } }",
                "Compilation of contract definition failed."
            );
        } else {
            assert!(false, "Unexpected error.");
        }
    }

    #[test]
    fn test_custom_type_in_function() {
        let vyp_code = "type CustomType = u256\
                      \n
                      \ncontract Foo:\
                      \n  pub def bar(x: CustomType) -> CustomType:\
                      \n    return x";
        let toks = vyper_parser::get_parse_tokens(vyp_code).unwrap();
        let stmt = parsers::file_input(&toks[..]).unwrap().1.node;

        if let Ok(Some(statement)) = module(&stmt) {
            assert_eq!(
                statement.to_string(),
                "{ object \"Foo\" { function bar(x:u256) -> return_val { return_val := x } } }",
                "Compilation of module definition failed."
            );
        } else {
            assert!(false, "Unexpected error.");
        }
    }
}
