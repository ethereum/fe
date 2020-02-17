use crate::errors::CompileError;
use crate::yul::{base, constructor, selectors};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

type Shared<T> = Rc<RefCell<T>>;

#[allow(dead_code)]
struct ModuleScope<'a> {
    type_defs: HashMap<&'a str, &'a vyp::TypeDesc<'a>>,
}

#[allow(dead_code)]
struct ContractScope<'a> {
    parent: Shared<ModuleScope<'a>>,
}

#[allow(dead_code)]
struct FunctionScope<'a> {
    parent: Shared<ContractScope<'a>>,
}

#[allow(dead_code)]
enum Scope<'a> {
    Module(Shared<ModuleScope<'a>>),
}

impl<'a> ModuleScope<'a> {
    fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            type_defs: HashMap::new(),
        }))
    }
}

impl<'a> ContractScope<'a> {
    fn new(parent: Shared<ModuleScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope { parent }))
    }
}

impl<'a> FunctionScope<'a> {
    fn new(parent: Shared<ContractScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(FunctionScope { parent }))
    }
}

pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<yul::Object, CompileError> {
    let scope = ModuleScope::new();

    let mut statements = module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
        .into_iter()
        .filter_map(|statement| statement)
        .collect::<Vec<yul::Statement>>();

    // TODO: Handle multiple objects(contracts)
    if let Some(yul::Statement::Object(object)) = statements.pop() {
        return Ok(object);
    }

    Err(CompileError::static_str(
        "Final statement in Yul module is not an object.",
    ))
}

fn module_stmt<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => {
            type_def(scope, stmt)?;
            Ok(None)
        }
        vyp::ModuleStmt::ContractDef { .. } => contract_def(scope, stmt).map(|s| Some(s)),
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

fn contract_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    let new_scope = ContractScope::new(scope);

    if let vyp::ModuleStmt::ContractDef { name: _, body } = stmt {
        let mut statements = body
            .iter()
            .map(|stmt| contract_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        // TODO: Use functions from actual contract ABI once the builder is merged.
        statements.push(yul::Statement::Switch(selectors::switch(&vec![])?));

        let def = yul::Object {
            name: base::untyped_identifier("Contract"),
            code: constructor::code(),
            objects: vec![yul::Object {
                name: base::untyped_identifier("runtime"),
                code: yul::Code {
                    block: yul::Block { statements },
                },
                objects: vec![],
            }],
        };

        return Ok(yul::Statement::Object(def));
    }

    Err(CompileError::static_str(
        "Contract definition translation requires ContractDef.",
    ))
}

fn contract_stmt<'a>(
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::ContractStmt::FuncDef { .. } => func_def(scope, stmt),
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

fn type_def<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<(), CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        scope.borrow_mut().type_defs.insert(name.node, &typ.node);
        return Ok(());
    }

    Err(CompileError::static_str(
        "Type definition translation requires TypeDef.",
    ))
}

fn func_def<'a>(
    scope: Shared<ContractScope<'a>>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual: _,
        name,
        args,
        return_type,
        body,
    } = stmt
    {
        let new_scope = FunctionScope::new(scope);

        let parameters = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&new_scope), &arg.node))
            .collect::<Result<Vec<yul::Identifier>, CompileError>>()?;

        let returns = if return_type.is_some() {
            vec![base::untyped_identifier("return_value")]
        } else {
            Vec::new()
        };

        let statements: Vec<yul::Statement> = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        return Ok(yul::Statement::FunctionDefinition(
            yul::FunctionDefinition {
                name: base::untyped_identifier(name.node),
                parameters,
                returns,
                block: yul::Block { statements },
            },
        ));
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef.",
    ))
}

fn func_def_arg<'a>(
    _scope: Shared<FunctionScope<'a>>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<yul::Identifier, CompileError> {
    Ok(base::untyped_identifier(arg.name.node))
}

fn func_stmt<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        _ => Err(CompileError::static_str(
            "Unable to translate function statement",
        )),
    }
}

fn func_return<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Return { value: Some(value) } = stmt {
        let return_value = expr(scope, &value.node)?;

        return Ok(yul::Statement::Assignment(yul::Assignment {
            identifiers: vec![base::untyped_identifier("return_val")],
            expression: return_value,
        }));
    }

    Err(CompileError::static_str(
        "Function return translation requires Return parameter.",
    ))
}

fn expr<'a>(
    _scope: Shared<FunctionScope<'a>>,
    expr: &'a vyp::Expr<'a>,
) -> Result<yul::Expression, CompileError> {
    match expr {
        vyp::Expr::Name(name) => Ok(base::untyped_literal_expr(name)),
        _ => Err(CompileError::static_str("Unable to translate expression")),
    }
}

#[cfg(test)]
mod tests {

    /*
    #[test]
    fn test_compile_type_def() {
        let toks = vyper_parser::get_parse_tokens("type Num = u256").unwrap();
        let stmt = parsers::type_def(&toks[..]).unwrap().1.node;

        let mut scope = ModuleScope::new();

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

        let mut scope = ContractScope::new(ModuleScope::new());

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
        let vyp_code = "pub def bar(x: CustomType) -> CustomType:\
                        \n  return x";
        let toks = vyper_parser::get_parse_tokens(vyp_code).unwrap();
        let stmt = parsers::func_def(&toks[..]).unwrap().1.node;

        let mut module_scope = ModuleScope::new().into_shared();
        let u256 = TypeDesc::Base { base: "u256" };
        module_scope
            .borrow_mut()
            .type_defs
            .insert("CustomType", &u256);
        let scope = ContractScope::new(module_scope).into_shared();

        if let Ok(Some(statement)) = func_def(scope, &stmt) {
            assert_eq!(
                statement.to_string(),
                "function bar(x:u256) -> return_val { return_val := x }",
                "Compilation of module definition failed."
            );
        } else {
            assert!(false, "Unexpected error.");
        }
    }
    */
}
