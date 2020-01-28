use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

pub struct CompileError;

pub type CompileResult<'a, T> = Result<Option<T>, CompileError>;
type Shared<T> = Rc<RefCell<T>>;
type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;

struct ModuleScope<'a> {
    type_defs: TypeDefs<'a>,
}

struct ContractScope<'a> {
    parent: Shared<ModuleScope<'a>>,
    stor_refs: HashMap<&'a str, (usize, &'a vyp::TypeDesc<'a>)>,
    stor_size: usize,
}

struct FunctionScope<'a> {
    parent: Shared<ContractScope<'a>>,
}

enum SharedScope<'a> {
    Module(Shared<ModuleScope<'a>>),
    Contract(Shared<ContractScope<'a>>),
    Function(Shared<FunctionScope<'a>>),
}

impl<'a> ModuleScope<'a> {
    fn new() -> ModuleScope<'a> {
        ModuleScope {
            type_defs: HashMap::new(),
        }
    }

    fn into_shared(self) -> Shared<ModuleScope<'a>> {
        Rc::new(RefCell::new(self))
    }
}

impl<'a> ContractScope<'a> {
    fn new(parent: Shared<ModuleScope<'a>>) -> ContractScope<'a> {
        ContractScope {
            parent,
            stor_refs: HashMap::new(),
            stor_size: 0,
        }
    }

    fn into_shared(self) -> Shared<ContractScope<'a>> {
        Rc::new(RefCell::new(self))
    }
}

impl<'a> FunctionScope<'a> {
    fn new(parent: Shared<ContractScope<'a>>) -> FunctionScope<'a> {
        FunctionScope { parent }
    }

    fn into_shared(self) -> Shared<FunctionScope<'a>> {
        Rc::new(RefCell::new(self))
    }
}

pub fn module<'a>(module: &'a vyp::Module<'a>) -> CompileResult<'a, yul::Statement> {
    let mut scope = ModuleScope::new().into_shared();

    let statements_result: Result<Vec<Option<yul::Statement>>, CompileError> = module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect();

    let statements = statements_result?.into_iter().filter_map(|s| s).collect();

    Ok(Some(yul::Statement::Block(yul::Block { statements })))
}

fn module_stmt<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => type_def(scope, stmt),
        vyp::ModuleStmt::ContractDef { .. } => contract_def(scope, stmt),
        _ => Err(CompileError),
    }
}

fn contract_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    let new_scope = ContractScope::new(scope).into_shared();

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
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::ContractStmt::FuncDef { .. } => func_def(scope, stmt),
        vyp::ContractStmt::ContractField { .. } => contract_field(scope, stmt),
        _ => Err(CompileError),
    }
}

fn contract_field<'a>(
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt,
) -> CompileResult<'a, yul::Statement> {
    if let vyp::ContractStmt::ContractField { qual, name, typ } = stmt {
        return match &typ.node {
            vyp::TypeDesc::Array {
                typ: elem_typ,
                dimension,
            } => {
                let p = scope.borrow().stor_size;
                scope
                    .borrow_mut()
                    .stor_refs
                    .insert(name.node, (p, &typ.node));
                scope.borrow_mut().stor_size += *dimension;
                Ok(Some(yul::Statement::Assignment(yul::Assignment {
                    identifiers: vec![yul::Identifier {
                        identifier: String::from(name.node),
                        yultype: None,
                    }],
                    expression: yul::Expression::Literal(yul::Literal {
                        literal: p.to_string(),
                        yultype: Some(yul::Type::Uint256),
                    }),
                })))
            }
            _ => Err(CompileError),
        };
    }

    return Err(CompileError);
}

fn type_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        scope.borrow_mut().type_defs.insert(name.node, &typ.node);
        return Ok(None);
    }

    Err(CompileError)
}

fn func_def<'a>(
    scope: Shared<ContractScope<'a>>,
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
        let new_scope = FunctionScope::new(scope).into_shared();

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
    scope: Shared<FunctionScope<'a>>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> CompileResult<'a, yul::Identifier> {
    Ok(Some(yul::Identifier {
        identifier: String::from(arg.name.node),
        yultype: type_desc(SharedScope::Function(scope), &arg.typ.node)?,
    }))
}

fn func_stmt<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> CompileResult<'a, yul::Statement> {
    match stmt {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        _ => Err(CompileError),
    }
}

fn func_return<'a>(
    scope: Shared<FunctionScope<'a>>,
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
    mut desc: &'a vyp::TypeDesc<'a>,
) -> CompileResult<'a, yul::Type> {
    // Custom types are stored in `TypeDesc::Base`
    if let vyp::TypeDesc::Base { base } = desc {
        if let SharedScope::Function(scope) = scope {
            // TODO: Clean this up.
            if let Some(custom_desc) = scope
                .borrow()
                .parent
                .borrow()
                .parent
                .borrow()
                .type_defs
                .get(base)
            {
                desc = custom_desc;
            }
        }
    }

    Ok(Some(match desc {
        vyp::TypeDesc::Base { base: "u256" } => yul::Type::Uint256,
        vyp::TypeDesc::Array { .. } => yul::Type::Uint256,
        _ => return Err(CompileError),
    }))
}

fn expr<'a>(
    scope: Shared<FunctionScope<'a>>,
    expr: &'a vyp::Expr<'a>,
) -> CompileResult<'a, yul::Expression> {
    Ok(Some(match expr {
        vyp::Expr::Name(name) => yul::Expression::Identifier(yul::Identifier {
            identifier: String::from(*name),
            yultype: None,
        }),
        _ => return Err(CompileError),
    }))
}

#[cfg(test)]
mod tests {
    use crate::yul::{contract_def, func_def, module, type_def, ModuleScope, ContractScope};
    use std::cell::RefCell;
    use std::rc::Rc;
    use vyper_parser::ast::TypeDesc;
    use vyper_parser::parsers;

    #[test]
    fn test_compile_type_def() {
        let toks = vyper_parser::get_parse_tokens("type Num = u256").unwrap();
        let stmt = parsers::type_def(&toks[..]).unwrap().1.node;

        let mut scope = ModuleScope::new().into_shared();

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

        let mut scope = ContractScope::new(ModuleScope::new().into_shared()).into_shared();

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

        let mut scope = ModuleScope::new().into_shared();

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
