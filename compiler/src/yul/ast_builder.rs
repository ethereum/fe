use crate::errors::CompileError;
use crate::yul::{
    base, constructor, types,
    runtime::{abi as runtime_abi, functions as runtime_functions},
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use stringreader::StringReader;
use vyper_parser::ast as vyp;
use yultsur::*;
use crate::yul::ast_builder::ModuleDef::Type;

pub type Shared<T> = Rc<RefCell<T>>;

pub enum ModuleDef {
    Type(types::Type)
}

pub enum ContractDef {
    Function { params: Vec<types::FixedSize>, returns: Option<types::FixedSize> },
    Map(usize, types::Map),
    Array(usize, types::Array),
}

pub enum FunctionDef {
    Base(types::Base),
    Array(types::Array),
}

pub struct ModuleScope {
    defs: HashMap<String, ModuleDef>,
}

pub struct ContractScope {
    parent: Shared<ModuleScope>,
    defs: HashMap<String, ContractDef>,
    interface: Vec<String>,
    storage_count: usize,
}

pub struct FunctionScope {
    parent: Shared<ContractScope>,
    defs: HashMap<String, FunctionDef>
}

#[allow(dead_code)]
pub enum Scope {
    Module(Shared<ModuleScope>),
    Contract(Shared<ContractScope>),
    Function(Shared<FunctionScope>),
}

impl Scope {
    pub fn module_scope(&self) -> Shared<ModuleScope> {
        match self {
            Scope::Module(scope) => Rc::clone(scope),
            Scope::Contract(scope) => Rc::clone(&scope.borrow().parent),
            Scope::Function(scope) => Rc::clone(&scope.borrow().parent.borrow().parent),
        }
    }
}

impl ModuleScope {
    fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            defs: HashMap::new(),
        }))
    }

    pub fn add_type_def(&mut self, name: String, typ: types::Type) {
        self.defs.insert(name, ModuleDef::Type(typ));
    }
}

impl ContractScope {
    fn new(parent: Shared<ModuleScope>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope {
            parent,
            defs: HashMap::new(),
            interface: vec![],
            storage_count: 0,
        }))
    }

    pub fn add_map(&mut self, name: String, map: types::Map) {
        self.defs.insert(name, ContractDef::Map(self.storage_count, map));
        self.storage_count += 1;
    }

    pub fn add_function(
        &mut self,
        name: String,
        params: Vec<types::FixedSize>,
        returns: Option<types::FixedSize>,
    ) {
        self.interface.push(name.clone());
        self.defs.insert(name, ContractDef::Function { params, returns });
    }
}

impl FunctionScope {
    fn new(parent: Shared<ContractScope>) -> Shared<Self> {
        Rc::new(RefCell::new(FunctionScope {
            parent,
            defs: HashMap::new(),
        }))
    }

    pub fn module_scope(&self) -> Shared<ModuleScope> {
        Rc::clone(&self.parent.borrow().parent)
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
    scope: Shared<ModuleScope>,
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
    module_scope: Shared<ModuleScope>,
    def: &'a vyp::ModuleStmt<'a>,
) -> Result<yul::Object, CompileError> {
    let contract_scope = ContractScope::new(Rc::clone(&module_scope));

    if let vyp::ModuleStmt::ContractDef { name: _, body } = def {
        let mut statements = body
            .iter()
            .map(|stmt| contract_stmt(Rc::clone(&contract_scope), &stmt.node))
            .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
            .into_iter()
            .filter_map(|stmt| stmt)
            .collect::<Vec<yul::Statement>>();

        statements.append(&mut runtime_functions::all());
        statements.push(runtime_abi::switch(
            &contract_scope.borrow().interface,
            &contract_scope.borrow().defs
        )?);

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
    scope: Shared<ContractScope>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ContractStmt::ContractField { .. } => {
            contract_field(scope, stmt)?;
            Ok(None)
        },
        vyp::ContractStmt::FuncDef { .. } => func_def(scope, stmt).map(|s| Some(s)),
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

/// Creates a new contract variable entry from a Vyper contract field
pub fn contract_field<'a>(
    scope: Shared<ContractScope>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<(), CompileError> {
    if let vyp::ContractStmt::ContractField { qual: _, name, typ } = stmt {
        match type_desc(Scope::Contract(Rc::clone(&scope)), &typ.node)? {
            types::Type::Map(map) => {
                scope.borrow_mut().add_map(name.node.to_string(), map);
            },
            _ => return Err(CompileError::static_str("Contract field type not supported"))
        };

        return Ok(());
    }

    Err(CompileError::static_str("Not a contract field"))
}

/// Adds the custom type def to the module scope.
pub fn type_def<'a>(
    scope: Shared<ModuleScope>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<(), CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        let typ = types::type_desc(&scope.borrow().defs, &typ.node)?;
        scope.borrow_mut().defs.insert(name.node.to_string(), ModuleDef::Type(typ));
        return Ok(());
    }

    Err(CompileError::static_str(
        "Type definition translation requires TypeDef.",
    ))
}

/// Builds a Yul function definition from a Vyper function definition.
pub fn func_def<'a>(
    contract_scope: Shared<ContractScope>,
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
        let function_scope = FunctionScope::new(Rc::clone(&contract_scope));

        let returns = return_type.as_ref().map(|r| type_desc_fixed_size(
            Scope::Function(Rc::clone(&function_scope)),
            &r.node
        ).unwrap());

        let (param_identifiers, param_types) = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&function_scope), &arg.node))
            .collect::<Result<Vec<(yul::Identifier, types::FixedSize)>, CompileError>>()?
            .into_iter()
            .fold((vec![], vec![]), |(mut is, mut ts), (i, t)| {
                is.push(i);
                ts.push(t);
                (is, ts)
            });

        contract_scope.borrow_mut().add_function(
            name.node.to_string(),
            param_types,
            returns.clone()
        );

        let name = base::untyped_identifier(name.node);

        let function_statements = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&function_scope), &stmt.node))
            .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

        return match returns {
            None => {
                Ok(function_definition! {
                    function [name]([param_identifiers...]) {
                        (let ptr := avail())
                        [function_statements...]
                        (free(ptr))
                    }
                })
            }
            Some(types::FixedSize::Base(_)) => {
                Ok(function_definition! {
                    function [name]([param_identifiers...]) -> return_val {
                        (let ptr := avail())
                        [function_statements...]
                        (free(ptr))
                    }
                })
            }
            Some(types::FixedSize::Array(array)) => {
                let size = base::untyped_literal_expr(&array.size().to_string());

                Ok(function_definition! {
                    function [name]([param_identifiers...]) -> return_val {
                        [function_statements...]
                        (free((add(return_val, [size]))))
                    }
                })
            }
            _ => {
                Err(CompileError::static_str(
                    "Return no supported for this location.",
                ))
            }
        };
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef.",
    ))
}

/// Builds a Yul identifier from a Vyper function argument.
pub fn func_def_arg<'a>(
    scope: Shared<FunctionScope>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<(yul::Identifier, types::FixedSize), CompileError> {
    Ok((base::untyped_identifier(arg.name.node), type_desc_fixed_size(Scope::Function(scope), &arg.typ.node)?))
}

/// Builds a Yul statement from a function statement.
pub fn func_stmt<'a>(
    scope: Shared<FunctionScope>,
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
    scope: Shared<FunctionScope>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::VarDecl { target, typ, value } = stmt {
        return match &typ.node {
            vyp::TypeDesc::Base { .. } => var_decl_base(scope, stmt),
            vyp::TypeDesc::Array { .. } => var_decl_array(scope, stmt),
            vyp::TypeDesc::Map { .. } => Err(CompileError::static_str("Cannot declare map in function")),
        }
    }

    Err(CompileError::static_str("Not a VarDecl statement"))
}

pub fn var_decl_base<'a>(
    scope: Shared<FunctionScope>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::VarDecl { target, typ, value } = stmt {
        let identifier = expr_name(&target.node)?;

        return if let Some(ref value) = value {
            let initial_value = expr(scope, &target.node)?;
            Ok(statement! { let [identifier] := [initial_value] })
        } else {
            Ok(statement! { let [identifier] := 0 })
        };
    }

    Err(CompileError::static_str("Not a variable declaration"))
}

pub fn var_decl_array<'a>(
    scope: Shared<FunctionScope>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::VarDecl { target, typ, value: _ } = stmt {
        let typ = type_desc_array(Scope::Function(scope), &typ.node)?;
        let identifier = expr_name(&target.node)?;
        let size = base::untyped_literal_expr(&typ.size().to_string());

        return Ok(statement! { let [identifier] := alloc([size]) });
    }

    Err(CompileError::static_str("Not a variable declaration"))
}

/// Builds a Yul return statement from a Vyper return statement.
pub fn func_return<'a>(
    scope: Shared<FunctionScope>,
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
    scope: Shared<FunctionScope>,
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
    scope: Shared<FunctionScope>,
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
    _scope: Shared<FunctionScope>,
    expr: &'a vyp::Expr<'a>,
) -> Result<yul::Expression, CompileError> {
    match expr {
        vyp::Expr::Name(name) => Ok(base::untyped_identifier_expr(name)),
        vyp::Expr::Num(num) => Ok(base::untyped_literal_expr(num)),
        _ => Err(CompileError::static_str("Unable to translate expression")),
    }
}

pub fn expr_name<'a>(expr: &'a vyp::Expr<'a>) -> Result<yul::Identifier, CompileError> {
    if let vyp::Expr::Name(name) = expr {
        return Ok(base::untyped_identifier(name));
    }

    Err(CompileError::static_str("Not a name expression."))
}

pub fn expr_name_str<'a>(expr: &'a vyp::Expr<'a>) -> Result<&'a str, CompileError> {
    if let vyp::Expr::Name(name) = expr {
        return Ok(name);
    }

    Err(CompileError::static_str("Not a name expression."))
}

pub fn type_desc<'a>(
    scope: Scope,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<types::Type, CompileError> {
    types::type_desc(&scope.module_scope().borrow().defs, typ)
}

pub fn type_desc_array<'a>(
    scope: Scope,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<types::Array, CompileError> {
    types::type_desc_array(&scope.module_scope().borrow().defs, typ)
}

pub fn type_desc_fixed_size<'a>(
    scope: Scope,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<types::FixedSize, CompileError> {
    types::type_desc_fixed_size(&scope.module_scope().borrow().defs, typ)
}

#[cfg(test)]
mod tests {
    use crate::yul::ast_builder::{type_def, ModuleScope};
    use std::rc::Rc;
    use vyper_parser::ast::TypeDesc;
    use vyper_parser::parsers;

    /*
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
    */

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
