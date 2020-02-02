use serde::{Serialize};
use vyper_parser::ast as vyp;

#[derive(Serialize)]
pub struct Contract {
    functions: Vec<Function>
}

#[derive(Serialize)]
struct Function {
    name: String,
    #[serde(rename = "type")]
    typ: FunctionType,
    inputs: Vec<Input>,
    outputs: Vec<Output>
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

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum FunctionType {
    Function,
    Constructor,
    Receive,
    Fallback
}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum VariableType {
    Uint256
}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable
}

pub fn contract_def<'a>(stmt: &'a vyp::ModuleStmt<'a>) -> Result<Contract, &str> {
    if let vyp::ModuleStmt::ContractDef {name, body} = stmt {
        return Ok(Contract { // TODO: filter out non-pub functions
            functions: body.iter().map(|stmt| {
                func_def(&stmt.node)
            }).collect::<Result<Vec<Function>, &str>>()?
        });
    }

    Err("Not a contract.")
}

fn func_def<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<Function, &str> {
    if let vyp::ContractStmt::FuncDef { qual, name, args, return_type, body } = stmt {
        return Ok(Function {
            name: String::from(name.node),
            typ: FunctionType::Function, // TODO: Actually map this.
            inputs: args.iter().map(|arg| {
                func_def_arg(&arg.node)
            }).collect(),
            outputs: Vec::new()
        })
    }

    Err("Not a function.")
}

fn func_def_arg<'a>(arg: &'a vyp::FuncDefArg<'a>) -> Input {
    Input {
        name: String::from(arg.name.node),
        typ: VariableType::Uint256 // TODO: map this as well
    }
}

#[test]
fn test_function_serialize() {
    let func = Function {
        name: String::from("foobar"),
        typ: FunctionType::Function,
        inputs: vec![
            Input {
                name: String::from("foo"),
                typ: VariableType::Uint256
            }
        ],
        outputs: vec![
            Output {
                name: String::from("bar"),
                typ: VariableType::Uint256
            }
        ]
    };

    let json = serde_json::to_string(&func).unwrap();

    assert_eq!(
        json,
        "{\"name\":\"foobar\",\"type\":\"function\",\"inputs\":[{\"name\":\"foo\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"bar\",\"type\":\"uint256\"}]}",
        "Serialized function not correct."
    )
}