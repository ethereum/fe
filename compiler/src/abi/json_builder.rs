use crate::errors::CompileError;
use core::fmt;
use serde::export::fmt::Error;
use serde::export::Formatter;
use serde::Serialize;
use std::collections::HashMap;
use vyper_parser::ast as vyp;

/// Used to keep track of custom types defined in a module.
pub type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;

/// TODO: Add support for events.
#[derive(Serialize, Debug, PartialEq)]
pub struct Contract {
    pub functions: Vec<Function>,
}

#[derive(Serialize, Debug, PartialEq)]
pub struct Function {
    name: String,
    #[serde(rename = "type")]
    typ: FunctionType,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
}

#[derive(Serialize, Debug, PartialEq)]
pub struct Input {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[derive(Serialize, Debug, PartialEq)]
pub struct Output {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Constructor,
    Receive,
    Fallback,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableType {
    Uint256,
    Address,
    FixedBytes(usize),
    FixedArray(Box<VariableType>, usize),
}

impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            VariableType::Uint256 => write!(f, "uint256"),
            VariableType::Address => write!(f, "address"),
            VariableType::FixedBytes(size) => write!(f, "bytes{}", size),
            VariableType::FixedArray(inner, dim) => write!(f, "{}[{}]", inner, dim),
        }
    }
}

impl serde::Serialize for VariableType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq)]
pub enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

/// Builds a vector containing contract ABIs.
pub fn module<'a>(module: &'a vyp::Module) -> Result<Vec<Contract>, CompileError> {
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

/// Builds a contract ABI.
pub fn contract_def<'a>(
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

/// Builds an ABI function element, returning None if the function is private.
pub fn func_def<'a>(
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
                name: "".to_string(),
                typ: type_desc(type_defs, &return_type.node)?,
            }]
        } else {
            vec![]
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

    Ok(None)
}

/// Builds a function input.
pub fn func_def_arg<'a>(
    type_defs: &'a TypeDefs<'a>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<Input, CompileError> {
    Ok(Input {
        name: String::from(arg.name.node),
        typ: type_desc(&type_defs, &arg.typ.node)?,
    })
}

/// Maps the type description to an ABI-friendly type and handles custom types.
pub fn type_desc<'a>(
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
        vyp::TypeDesc::Base { base: "address" } => Ok(VariableType::Address),
        vyp::TypeDesc::Array { typ, dimension } => {
            if let vyp::TypeDesc::Base { base: "bytes" } = &typ.node {
                return Ok(VariableType::FixedBytes(*dimension));
            }

            let inner = type_desc(type_defs, &typ.node)?;
            Ok(VariableType::FixedArray(Box::new(inner), *dimension))
        }
        _ => Err(CompileError::static_str("Unrecognized Vyper type")),
    }
}

#[cfg(test)]
mod tests {
    use crate::abi::json_builder::{
        func_def, type_desc, Function, FunctionType, Input, Output, VariableType,
    };
    use std::collections::HashMap;
    use vyper_parser::ast as vyp;
    use vyper_parser::parsers;

    #[test]
    fn test_type_desc_custom() {
        let toks = vyper_parser::get_parse_tokens("CustomType").unwrap();
        let vyp_type_desc = parsers::type_desc(&toks[..]).unwrap().1.node;
        let mut type_defs = HashMap::new();
        type_defs.insert("CustomType", &vyp::TypeDesc::Base { base: "u256" });

        let variable_type = type_desc(&type_defs, &vyp_type_desc).unwrap();
        assert_eq!(variable_type, VariableType::Uint256);
    }

    #[test]
    fn test_type_desc_array() {
        let toks = vyper_parser::get_parse_tokens("u256[5]").unwrap();
        let vyp_type_desc = parsers::type_desc(&toks[..]).unwrap().1.node;
        let type_defs = HashMap::new();

        let variable_type = type_desc(&type_defs, &vyp_type_desc).unwrap();
        assert_eq!(
            variable_type.clone(),
            VariableType::FixedArray(Box::new(VariableType::Uint256), 5)
        );
        assert_eq!(variable_type.to_string(), "uint256[5]");
    }

    #[test]
    fn test_func_def() {
        let toks = vyper_parser::get_parse_tokens("pub def foo(x: u256) -> u256[10]:\n   return x")
            .unwrap();
        let vyp_func_def = parsers::func_def(&toks[..]).unwrap().1.node;

        let function = func_def(&HashMap::new(), &vyp_func_def).unwrap().unwrap();
        let expected = Function {
            name: "foo".to_string(),
            typ: FunctionType::Function,
            inputs: vec![Input {
                name: "x".to_string(),
                typ: VariableType::Uint256,
            }],
            outputs: vec![Output {
                name: "".to_string(),
                typ: VariableType::FixedArray(Box::new(VariableType::Uint256), 10),
            }],
        };
        assert_eq!(function, expected);
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
                name: String::from(""),
                typ: VariableType::FixedArray(Box::new(VariableType::Uint256), 10),
            }],
        };

        let json = serde_json::to_string(&func).unwrap();
        assert_eq!(
            json,
            r#"{"name":"foobar","type":"function","inputs":[{"name":"foo","type":"uint256"}],"outputs":[{"name":"","type":"uint256[10]"}]}"#,
        )
    }
}
