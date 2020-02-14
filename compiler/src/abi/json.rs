use crate::errors::CompileError;
use serde::Serialize;
use std::collections::HashMap;
use vyper_parser as parser;
use vyper_parser::ast as vyp;

type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;

#[derive(Serialize)]
pub struct Contract {
    functions: Vec<Function>,
}

#[derive(Serialize)]
struct Function {
    name: String,
    #[serde(rename = "type")]
    typ: FunctionType,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
}

#[derive(Serialize)]
struct Input {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[derive(Serialize)]
struct Output {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize)]
enum FunctionType {
    Function,
    Constructor,
    Receive,
    Fallback,
}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum VariableType {
    Uint256,
}

impl ToString for VariableType {
    fn to_string(&self) -> String {
        match self {
            VariableType::Uint256 => String::from("uint256"),
        }
    }
}

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize)]
enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src).unwrap();
    let vyp_module = parser::parsers::file_input(&tokens[..]).unwrap().1.node;
    let contracts = module(&vyp_module)?;

    // TODO: Handle multiple contracts in one source file.
    if let Some(contract) = contracts.get(0) {
        return Ok(serde_json::to_string(&contract.functions)?);
    }

    Err(CompileError::static_str(
        "No contract statements in source.",
    ))
}

fn module<'a>(module: &'a vyp::Module) -> Result<Vec<Contract>, CompileError> {
    let type_defs: TypeDefs<'a> = module
        .body
        .iter()
        .filter_map(|stmt| match stmt.node {
            vyp::ModuleStmt::TypeDef { ref name, ref typ } => Some((name.node, &typ.node)),
            _ => None,
        })
        .collect();

    module
        .body
        .iter()
        .filter_map(|stmt| match stmt.node {
            vyp::ModuleStmt::ContractDef { .. } => Some(contract_def(&type_defs, &stmt.node)),
            _ => None,
        })
        .collect::<Result<Vec<Contract>, CompileError>>()
}

fn contract_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Contract, CompileError> {
    if let vyp::ModuleStmt::ContractDef { name: _, body } = stmt {
        return Ok(Contract {
            functions: body
                .iter()
                .map(|stmt| func_def(type_defs, &stmt.node))
                .collect::<Result<Vec<Option<Function>>, CompileError>>()?
                .into_iter()
                .filter_map(|function| function)
                .collect(),
        });
    }

    Err(CompileError::static_str(
        "Contract definition translation requires ContractDef parameter.",
    ))
}

fn func_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Option<Function>, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual,
        name,
        args,
        return_type,
        body: _,
    } = stmt
    {
        if qual.is_none() {
            return Ok(None); // Private method.
        }

        let outputs = if let Some(return_type) = return_type {
            vec![Output {
                name: String::from("return_value"),
                typ: type_desc(type_defs, &return_type.node)?,
            }]
        } else {
            Vec::new()
        };

        return Ok(Some(Function {
            name: String::from(name.node),
            typ: FunctionType::Function,
            inputs: args
                .iter()
                .map(|arg| func_def_arg(type_defs, &arg.node))
                .collect::<Result<Vec<Input>, CompileError>>()?,
            outputs,
        }));
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef parameter.",
    ))
}

fn func_def_arg<'a>(
    type_defs: &'a TypeDefs<'a>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<Input, CompileError> {
    Ok(Input {
        name: String::from(arg.name.node),
        typ: type_desc(&type_defs, &arg.typ.node)?,
    })
}

fn type_desc<'a>(
    type_defs: &'a TypeDefs<'a>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<VariableType, CompileError> {
    if let vyp::TypeDesc::Base { base } = typ {
        if let Some(custom_type) = type_defs.get(base) {
            return type_desc(&HashMap::new(), custom_type);
        }
    }

    match typ {
        vyp::TypeDesc::Base { base: "u256" } => Ok(VariableType::Uint256),
        _ => Err(CompileError::static_str("Unrecognized Vyper type.")),
    }
}

#[test]
fn test_function_serialize() {
    let func = Function {
        name: String::from("foobar"),
        typ: FunctionType::Function,
        inputs: vec![Input {
            name: String::from("foo"),
            typ: VariableType::Uint256,
        }],
        outputs: vec![Output {
            name: String::from("bar"),
            typ: VariableType::Uint256,
        }],
    };

    let json = serde_json::to_string(&func).unwrap();

    assert_eq!(
        json,
        r#"{"name":"foobar","type":"function","inputs":[{"name":"foo","type":"uint256"}],"outputs":[{"name":"bar","type":"uint256"}]}"#,
        "Serialized function not correct."
    )
}
