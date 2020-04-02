use crate::errors::CompileError;
use core::fmt;
use serde::export::fmt::Error;
use serde::export::Formatter;
use serde::Serialize;
use std::collections::HashMap;
use vyper_parser::ast as vyp;

/// Used to keep track of custom types defined in a module.
pub type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;
pub type TypeDef<'a> = (&'a str, &'a vyp::TypeDesc<'a>);

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Contract {
    pub items: Vec<Item>,
}

#[serde(untagged)]
#[derive(Serialize, Debug, PartialEq, Clone)]
pub enum Item {
    Event(Event),
    Function(Function),
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Event {
    name: String,
    #[serde(rename = "type")]
    typ: String,
    inputs: Vec<EventField>,
    anonymous: bool,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct EventField {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
    indexed: bool,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Function {
    name: String,
    #[serde(rename = "type")]
    typ: FunctionType,
    inputs: Vec<FuncInput>,
    outputs: Vec<Output>,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncInput {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Output {
    name: String,
    #[serde(rename = "type")]
    typ: VariableType,
}

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq, Clone)]
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

#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq, Clone)]
pub enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
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

/// Builds a vector containing contract ABIs.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<Vec<Contract>, CompileError> {
    let type_defs = module
        .body
        .iter()
        .map(|s| type_def(&s.node))
        .collect::<Result<Vec<Option<TypeDef<'a>>>, CompileError>>()?
        .into_iter()
        .filter_map(|o| o)
        .collect::<TypeDefs<'a>>();

    Ok(module
        .body
        .iter()
        .map(|s| contract_def(&type_defs, &s.node))
        .collect::<Result<Vec<Option<Contract>>, CompileError>>()?
        .into_iter()
        .filter_map(|o| o)
        .collect::<Vec<Contract>>())
}

/// Builds a contract.
pub fn contract_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Option<Contract>, CompileError> {
    if let vyp::ModuleStmt::ContractDef { name: _, body } = stmt {
        let functions = body
            .iter()
            .map(|s| func_def(type_defs, &s.node))
            .collect::<Result<Vec<Option<Item>>, CompileError>>()?
            .into_iter()
            .filter_map(|o| o)
            .collect::<Vec<Item>>();

        let events = body
            .iter()
            .map(|s| event_def(type_defs, &s.node))
            .collect::<Result<Vec<Option<Item>>, CompileError>>()?
            .into_iter()
            .filter_map(|o| o)
            .collect::<Vec<Item>>();

        return Ok(Some(Contract {
            items: [&functions[..], &events[..]].concat(),
        }));
    }

    Ok(None)
}

/// Builds an event item.
pub fn event_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Option<Item>, CompileError> {
    if let vyp::ContractStmt::EventDef { name, fields } = stmt {
        let inputs = fields
            .iter()
            .map(|f| event_field(type_defs, &f.node))
            .collect::<Result<_, _>>()?;

        return Ok(Some(Item::Event(Event {
            name: name.node.to_string(),
            typ: "event".to_string(),
            inputs,
            anonymous: false,
        })));
    }

    Ok(None)
}

/// Builds an event input.
pub fn event_field<'a>(
    type_defs: &'a TypeDefs<'a>,
    field: &'a vyp::EventField<'a>,
) -> Result<EventField, CompileError> {
    Ok(EventField {
        name: String::from(field.name.node),
        typ: type_desc(&type_defs, &field.typ.node)?,
        indexed: false,
    })
}

/// Builds a function item.
pub fn func_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Option<Item>, CompileError> {
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

        let inputs = args
            .iter()
            .map(|arg| func_def_arg(type_defs, &arg.node))
            .collect::<Result<Vec<FuncInput>, CompileError>>()?;

        let outputs = if let Some(return_type) = return_type {
            vec![Output {
                name: "".to_string(),
                typ: type_desc(type_defs, &return_type.node)?,
            }]
        } else {
            vec![]
        };

        return Ok(Some(Item::Function(Function {
            name: String::from(name.node),
            typ: FunctionType::Function,
            inputs,
            outputs,
        })));
    }

    Ok(None)
}

/// Builds a function input.
pub fn func_def_arg<'a>(
    type_defs: &'a TypeDefs<'a>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<FuncInput, CompileError> {
    Ok(FuncInput {
        name: String::from(arg.name.node),
        typ: type_desc(&type_defs, &arg.typ.node)?,
    })
}

/// Builds a type definition.
pub fn type_def<'a>(stmt: &'a vyp::ModuleStmt<'a>) -> Result<Option<TypeDef<'a>>, CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        return Ok(Some((name.node, &typ.node)));
    }

    Ok(None)
}

/// Maps the type description to an ABI type and handles custom types.
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
        vyp::TypeDesc::Base { base: "uint256" } => Ok(VariableType::Uint256),
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
        func_def, type_desc, FuncInput, Function, FunctionType, Item, Output, VariableType,
    };
    use std::collections::HashMap;
    use vyper_parser::ast as vyp;
    use vyper_parser::parsers;

    #[test]
    fn test_type_desc_custom() {
        let toks = vyper_parser::get_parse_tokens("CustomType").unwrap();
        let vyp_type_desc = parsers::type_desc(&toks[..]).unwrap().1.node;
        let mut type_defs = HashMap::new();
        type_defs.insert("CustomType", &vyp::TypeDesc::Base { base: "uint256" });

        let variable_type = type_desc(&type_defs, &vyp_type_desc).unwrap();
        assert_eq!(variable_type, VariableType::Uint256);
    }

    #[test]
    fn test_type_desc_array() {
        let toks = vyper_parser::get_parse_tokens("uint256[5]").unwrap();
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
        let toks =
            vyper_parser::get_parse_tokens("pub def foo(x: uint256) -> uint256[10]:\n   return x")
                .unwrap();
        let vyp_func_def = parsers::func_def(&toks[..]).unwrap().1.node;

        let function = func_def(&HashMap::new(), &vyp_func_def).unwrap().unwrap();
        let expected = Item::Function(Function {
            name: "foo".to_string(),
            typ: FunctionType::Function,
            inputs: vec![FuncInput {
                name: "x".to_string(),
                typ: VariableType::Uint256,
            }],
            outputs: vec![Output {
                name: "".to_string(),
                typ: VariableType::FixedArray(Box::new(VariableType::Uint256), 10),
            }],
        });
        assert_eq!(function, expected);
    }

    #[test]
    fn test_function_serialize() {
        let func = Function {
            name: String::from("foobar"),
            typ: FunctionType::Function,
            inputs: vec![FuncInput {
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
