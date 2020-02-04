use crate::abi::VariableType::Uint256;
use crate::errors::CompileError;
use serde::Serialize;
use tiny_keccak::{Hasher, Keccak};
use vyper_parser as parser;
use vyper_parser::ast as vyp;

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

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
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

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src).unwrap();
    let module = parser::parsers::file_input(&tokens[..]).unwrap().1.node;

    // TODO: don't just assume the first module statement is a contract
    // Also, handle type defs.
    let functions = contract_def(&module.body[0].node)?.functions;
    serde_json::to_string(&functions)
        .map_err(|_| CompileError::static_str("ABI serialization failed."))
}

fn contract_def<'a>(stmt: &'a vyp::ModuleStmt<'a>) -> Result<Contract, CompileError> {
    if let vyp::ModuleStmt::ContractDef { name, body } = stmt {
        return Ok(Contract {
            // TODO: filter out non-pub functions
            functions: body
                .iter()
                .map(|stmt| func_def(&stmt.node))
                .collect::<Result<Vec<Function>, CompileError>>()?,
        });
    }

    Err(CompileError::static_str(
        "Contract definition translation requires ContractDef parameter.",
    ))
}

fn func_def<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<Function, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual,
        name,
        args,
        return_type,
        body,
    } = stmt
    {
        return Ok(Function {
            name: String::from(name.node),
            typ: FunctionType::Function, // TODO: Actually map this.
            inputs: args.iter().map(|arg| func_def_arg(&arg.node)).collect(),
            outputs: Vec::new(),
        });
    }

    Err(CompileError::static_str(
        "Function definition translation requires FuncDef parameter.",
    ))
}

fn func_def_arg<'a>(arg: &'a vyp::FuncDefArg<'a>) -> Input {
    Input {
        name: String::from(arg.name.node),
        typ: VariableType::Uint256, // TODO: map this as well
    }
}

pub fn func_select<'a>(stmt: &'a vyp::ContractStmt<'a>) -> Result<[u8; 4], CompileError> {
    let abi = func_def(&stmt)?;
    let input_types = abi
        .inputs
        .iter()
        .map(|input| input.typ.to_string())
        .collect::<Vec<String>>();
    let sig = format!("{}({})", abi.name, input_types.join(","));

    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 4];
    keccak.update(sig.as_bytes());
    keccak.finalize(&mut selector);
    Ok(selector)
}

#[cfg(test)]
mod tests {
    use crate::abi::{func_select, Function, FunctionType, Input, Output, VariableType};
    use vyper_parser::parsers;

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
            "{\"name\":\"foobar\",\"type\":\"function\",\"inputs\":[{\"name\":\"foo\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"bar\",\"type\":\"uint256\"}]}",
            "Serialized function not correct."
        )
    }

    #[test]
    fn test_func_select() {
        let toks =
            vyper_parser::get_parse_tokens("def foo(x: u256, y: u256) -> u256:\n   return x")
                .unwrap();
        let stmt = parsers::func_def(&toks[..]).unwrap().1.node;

        let selector = hex::encode(func_select(&stmt).unwrap());

        assert_eq!(selector, "04bc52f8", "Function selector does not match.");
    }
}
