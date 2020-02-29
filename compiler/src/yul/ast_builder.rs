use crate::abi;
use crate::errors::CompileError;
use crate::yul::{
    base, constructor,
    runtime::{abi as runtime_abi, functions as runtime_functions},
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use stringreader::StringReader;
use vyper_parser::ast as vyp;
use yultsur::*;

pub type Shared<T> = Rc<RefCell<T>>;
pub type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;
pub type Variable<'a> = (&'a vyp::TypeDesc<'a>, Location);
pub type Variables<'a> = HashMap<&'a str, Variable<'a>>;

#[allow(dead_code)]
pub enum Location {
    Value,
    Memory(usize),
    Storage(usize, usize),
}

#[allow(dead_code)]
pub struct ModuleScope<'a> {
    type_defs: TypeDefs<'a>,
}

#[allow(dead_code)]
pub struct ContractScope<'a> {
    parent: Shared<ModuleScope<'a>>,
    storage_index: usize,
    variables: Variables<'a>,
}

#[allow(dead_code)]
pub struct FunctionScope<'a> {
    parent: Shared<ContractScope<'a>>,
    variables: Variables<'a>,
}

#[allow(dead_code)]
pub enum Scope<'a> {
    Module(Shared<ModuleScope<'a>>),
    Contract(Shared<ContractScope<'a>>),
    Function(Shared<FunctionScope<'a>>),
}

impl<'a> Scope<'a> {
    pub fn module_scope(&self) -> Shared<ModuleScope<'a>> {
        match self {
            Scope::Module(scope) => Rc::clone(scope),
            Scope::Contract(scope) => Rc::clone(&scope.borrow().parent),
            Scope::Function(scope) => Rc::clone(&scope.borrow().parent.borrow().parent),
        }
    }
}

impl<'a> ModuleScope<'a> {
    fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            type_defs: HashMap::new(),
        }))
    }

    fn defined_type(&self, desc: &'a vyp::TypeDesc<'a>) -> &'a vyp::TypeDesc<'a> {
        if let vyp::TypeDesc::Base { base } = desc {
            if let Some(desc) = self.type_defs.get(base) {
                return desc;
            }
        }

        desc
    }
}

impl<'a> ContractScope<'a> {
    fn new(parent: Shared<ModuleScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope {
            parent,
            storage_index: 0,
            variables: HashMap::new(),
        }))
    }
}

impl<'a> FunctionScope<'a> {
    fn new(parent: Shared<ContractScope<'a>>) -> Shared<Self> {
        Rc::new(RefCell::new(FunctionScope {
            parent,
            variables: HashMap::new(),
        }))
    }
}

/// Builds a vector of Yul contracts from a Vyper module.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<Vec<yul::Object>, CompileError> {
    let scope = ModuleScope::new();

    Ok(module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
        .into_iter()
        .filter_map(|statement| {
            if let Some(yul::Statement::Object(object)) = statement {
                return Some(object);
            }

            None
        })
        .collect::<Vec<yul::Object>>())
}

/// Builds a Yul statement from a Vyper module statement.
pub fn module_stmt<'a>(
    scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => {
            type_def(scope, stmt)?;
            Ok(None)
        }
        vyp::ModuleStmt::ContractDef { .. } => {
            contract_def(scope, stmt).map(|object| Some(yul::Statement::Object(object)))
        }
        _ => Err(CompileError::static_str("Unsupported module statement.")),
    }
}

/// Builds a Yul object from a Vyper contract.
pub fn contract_def<'a>(
    module_scope: Shared<ModuleScope<'a>>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<yul::Object, CompileError> {
    let new_scope = ContractScope::new(Rc::clone(&module_scope));

    if let vyp::ModuleStmt::ContractDef { name: _, body } = stmt {
        let mut statements = body
            .iter()
            .map(|stmt| contract_stmt(Rc::clone(&new_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        let json_abi = abi::build_contract(&module_scope.borrow().type_defs, stmt)?;
        let abi = ethabi::Contract::load(StringReader::new(&json_abi))?;
        let functions = abi.functions().collect();
        statements.push(runtime_abi::switch(functions)?);
        statements.append(&mut runtime_functions::all());

        return Ok(yul::Object {
            name: base::untyped_identifier("Contract"),
            code: constructor::code(),
            objects: vec![yul::Object {
                name: base::untyped_identifier("runtime"),
                code: yul::Code {
                    block: yul::Block { statements },
                },
                objects: vec![],
            }],
        });
    }

    Err(CompileError::static_str(
        "Contract definition translation requires ContractDef.",
    ))
}

/// Builds a Yul statement from a Vyper contract statement.
pub fn contract_stmt<'a>(
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

/// Adds the custom type def to the module scope.
pub fn type_def<'a>(
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

/// Builds a Yul function definition from a Vyper function definition.
pub fn func_def<'a>(
    contract_scope: Shared<ContractScope<'a>>,
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
        // Create a new function scope.
        let function_scope = FunctionScope::new(contract_scope);

        // Set the function name.
        let name = base::untyped_identifier(name.node);

        // Set a return variable if it exists.
        let return_variable = Some((return_type, Location::Value));
        /*
        return_type
        .map(|desc| scoped_type_desc(Scope::Function(Rc::clone(&function_scope)), &desc.node));*/

        // Map Vyper parameters to Yul identifiers.
        let parameters = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&function_scope), &arg.node))
            .collect::<Result<Vec<yul::Identifier>, CompileError>>()?;

        // Map Vyper function statements to Yul statements.
        let function_statements = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&function_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        return Ok(match return_variable {
            None => {
                function_definition! {
                    function [name]([parameters...]) {
                        (let ptr := avail())
                        [function_statements...]
                        (free(ptr))
                    }
                }
            }
            Some((_, Location::Value)) => {
                function_definition! {
                    function [name]([parameters...]) -> return_val {
                        (let ptr := avail())
                        [function_statements...]
                        (free(ptr))
                    }
                }
            }
            Some((_, Location::Memory(size))) => {
                let size = base::untyped_literal_expr(&size.to_string());

                function_definition! {
                    function [name]([parameters...]) -> return_val {
                        [function_statements...]
                        (free((add(return_val, [size]))))
                    }
                }
            }
            _ => {
                return Err(CompileError::static_str(
                    "Return no supported for this location.",
                ))
            }
        });
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef.",
    ))
}

/// Builds a Yul identifier from a Vyper function argument.
pub fn func_def_arg<'a>(
    _scope: Shared<FunctionScope<'a>>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<yul::Identifier, CompileError> {
    Ok(base::untyped_identifier(arg.name.node))
}

/// Builds a Yul statement from a function statement.
pub fn func_stmt<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        vyp::FuncStmt::VarDecl { .. } => var_decl(scope, stmt),
        vyp::FuncStmt::Assign { .. } => assign(scope, stmt),
        _ => Err(CompileError::static_str("Unsupported function statement")),
    }
}

/// Builds a Yul statement from a variable declaration.
pub fn var_decl<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::VarDecl { target, typ, value } = stmt {
        if let yul::Expression::Identifier(identifier) = expr(Rc::clone(&scope), &target.node)? {
            let variable = scoped_type_desc(
                Scope::Function(Rc::clone(&scope)),
                &typ.node
            )?;

            let initial_value = if let Some(value) = value {
                expr(Rc::clone(&scope), &value.node)?
            } else {
                base::untyped_literal_expr("0")
            };

            return match variable {
                (_, Location::Value) => Ok(statement! {let [identifier] := [initial_value]}),
                (_, Location::Memory(size)) => {
                    let size = base::untyped_literal_expr(&size.to_string());
                    Ok(statement! {let [identifier] := alloc([size])})
                }
                _ => Err(CompileError::static_str("Unsupported type in VarDecl.")),
            }
        }

        return Err(CompileError::static_str("Not an identifier"));
    }

    Err(CompileError::static_str("Not a VarDecl statement"))
}

/// Builds a Yul return statement from a Vyper return statement.
pub fn func_return<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Return { value: Some(value) } = stmt {
        let expression = expr(scope, &value.node)?;
        return Ok(statement! { return_val := [expression] });
    }

    Err(CompileError::static_str("Unable to map function return"))
}

/// Builds a Yul assignment statement.
pub fn assign<'a>(
    scope: Shared<FunctionScope<'a>>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Assign { targets, value } = stmt {
        return Ok(match &targets[0].node {
            vyp::Expr::Subscript { .. } => {
                let (identifier, index) = subscript(Rc::clone(&scope), &targets[0].node)?;
                let expression = expr(scope, &value.node)?;
                statement! {mstore((add([identifier], (mul([index], 32)))), [expression])}
            }
            _ => return Err(CompileError::static_str("Unable to handle assignment.")),
        });
    }

    Err(CompileError::static_str("Unable to map function return"))
}

/// Retrieves the identifier and index of a Vyper subscript as Yul expressions.
pub fn subscript<'a>(
    scope: Shared<FunctionScope<'a>>,
    expression: &'a vyp::Expr<'a>,
) -> Result<(yul::Expression, yul::Expression), CompileError> {
    if let vyp::Expr::Subscript { value, slices } = expression {
        let index = if let vyp::Slice::Index(index) = &slices.node.first().unwrap().node {
            expr(Rc::clone(&scope), index)?
        } else {
            base::untyped_literal_expr("0")
        };

        return Ok((expr(scope, &value.node)?, index));
    }

    Err(CompileError::static_str("Not a subscript"))
}

/// Builds a Yul expression from a Vyper expression.
pub fn expr<'a>(
    _scope: Shared<FunctionScope<'a>>,
    expr: &'a vyp::Expr<'a>,
) -> Result<yul::Expression, CompileError> {
    match expr {
        vyp::Expr::Name(name) => Ok(base::untyped_identifier_expr(name)),
        vyp::Expr::Num(num) => Ok(base::untyped_literal_expr(num)),
        _ => Err(CompileError::static_str("Unable to translate expression")),
    }
}

/// Creates a Variable(type, location) from a type description.
pub fn scoped_type_desc<'a>(
    scope: Scope<'a>,
    desc: &'a vyp::TypeDesc<'a>,
) -> Result<Variable<'a>, CompileError> {
    let module_scope = scope.module_scope();
    let desc = module_scope.borrow().defined_type(desc);

    match desc {
        vyp::TypeDesc::Base { .. } => Ok((desc, Location::Value)),
        vyp::TypeDesc::Array { typ, dimension } => {
            Ok((desc, Location::Memory(base_type_size(&typ.node)? * *dimension)))
        },
        _ => Err(CompileError::static_str("Unmatched type"))
    }
}

/// Returns the size of a base type.
pub fn base_type_size<'a>(desc: &'a vyp::TypeDesc<'a>) -> Result<usize, CompileError> {
    if let vyp::TypeDesc::Base { base } = desc {
        return match *base {
            "u256" => Ok(256),
            _ => Err(CompileError::static_str("Unmatched base type"))
        };
    }

    Err(CompileError::static_str("Not a base type"))
}

#[cfg(test)]
mod tests {
    use crate::yul::ast_builder::{type_def, ModuleScope};
    use std::rc::Rc;
    use vyper_parser::ast::TypeDesc;
    use vyper_parser::parsers;

    #[test]
    fn test_compile_type_def() {
        let src = "type Foo = u256";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::type_def(&toks[..]).unwrap().1.node;
        let scope = ModuleScope::new();

        type_def(Rc::clone(&scope), &stmt).expect("Unable to handle type def");

        assert_eq!(
            scope.borrow().type_defs["Foo"],
            &TypeDesc::Base { base: "u256" },
            "Compilation of type definition failed."
        );
    }

    /*
    #[test]
    fn test_compile_func_def() {
        let src = "def foo(x: u256) -> u256:\n   return x";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::func_def(&toks[..]).unwrap().1.node;
        let scope = ContractScope::new(ModuleScope::new());

        let result = func_def(Rc::clone(&scope), &stmt).expect("Unable to build func def");

        assert_eq!(
            result.to_string(),
            "function foo(x) -> return_val { return_val := x }",
            "Compilation of function definition failed."
        );
    }

    #[test]
    fn test_contract_def() {
        let src = "contract Foo:\
                   \n  pub def bar(x: u256) -> u256:\
                   \n    return x";
        let toks = vyper_parser::get_parse_tokens(src).unwrap();
        let stmt = parsers::contract_def(&toks[..]).unwrap().1.node;
        let scope = ModuleScope::new();

        let result = contract_def(scope, &stmt).expect("Unable to handle contract def");

        assert_eq!(
            result.to_string(),
            r#"object "Contract" { code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) } object "runtime" { code { function bar(x) -> return_val { return_val := x } switch shr(224, calldataload(0)) case 0x0423a132 { mstore(0, bar(calldataload(4))) return(0, 32) }  }  } }"#,
            "Compilation of contract definition failed."
        );
    }
    */
}
