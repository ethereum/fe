use crate::errors::CompileError;
use fe_analyzer::namespace::types::{
    AbiComponent,
    AbiEncoding,
    FixedSize,
};
use fe_analyzer::FunctionAttributes;
use serde::ser::SerializeSeq;
use serde::{
    Serialize,
    Serializer,
};
use std::collections::HashMap;

/// The ABIs for each contract in a Fe module.
pub type ModuleAbis = HashMap<String, Contract>;

/// All public interfaces of a Fe contract.
#[derive(Debug, PartialEq, Clone)]
pub struct Contract {
    /// All events defined in a contract.
    pub events: Vec<Event>,
    /// All public functions defined in a contract.
    pub functions: Vec<Function>,
}

impl Default for Contract {
    fn default() -> Self {
        Self::new()
    }
}

impl Contract {
    pub fn new() -> Self {
        Self {
            events: vec![],
            functions: vec![],
        }
    }
}

impl Contract {
    /// Serialize the contract into a valid JSON ABI.
    pub fn json(&self, prettify: bool) -> Result<String, CompileError> {
        match prettify {
            true => serde_json::to_string_pretty(self),
            false => serde_json::to_string(self),
        }
        .map_err(|_| CompileError::static_str("unable to serialize contract to json"))
    }
}

impl Serialize for Contract {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;

        for event in self.events.iter() {
            seq.serialize_element(event)?;
        }

        for function in self.functions.iter() {
            seq.serialize_element(function)?;
        }

        seq.end()
    }
}

/// An event interface.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Event {
    /// The event's name.
    pub name: String,
    /// The type of an event (Always "event").
    #[serde(rename = "type")]
    pub typ: String,
    /// All event fields.
    #[serde(rename = "inputs")]
    pub fields: Vec<EventField>,
    /// True if the event was declared as anonymous.
    pub anonymous: bool,
}

/// A single event field.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct EventField {
    /// The event field's name.
    pub name: String,
    /// The type of an event (e.g. u256, address, bytes100,...)
    #[serde(rename = "type")]
    pub typ: String,
    /// True if the field is part of the log’s topics, false if it is one of the
    /// log’s data segment.
    pub indexed: bool,
}

/// A function interface.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Function {
    /// The function's name.
    pub name: String,
    /// The type of a function (Function, Constructor, Receive, and Fallback)
    #[serde(rename = "type")]
    pub typ: FuncType,
    /// All function inputs.
    pub inputs: Vec<FuncInput>,
    /// All function outputs.
    pub outputs: Vec<FuncOutput>,
}

/// Component of an ABI tuple.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Component {
    name: String,
    #[serde(rename = "type")]
    typ: String,
}

impl From<AbiComponent> for Component {
    fn from(component: AbiComponent) -> Self {
        if !component.components.is_empty() {
            todo!("ABI serialization of subcomponents")
        }

        Self {
            name: component.name,
            typ: component.typ,
        }
    }
}

/// A single function input.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncInput {
    /// The input's name.
    pub name: String,
    /// The input's type.
    #[serde(rename = "type")]
    pub typ: String,
    /// Components of a tuple. This field is excluded if there are no
    /// components.
    #[serde(skip_serializing_if = "should_skip_components")]
    pub components: Vec<Component>,
}

/// A single function output.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncOutput {
    /// The output's name.
    pub name: String,
    /// The output's type.
    #[serde(rename = "type")]
    pub typ: String,
    /// Components of a tuple. This field is excluded if there are no
    /// components.
    #[serde(skip_serializing_if = "should_skip_components")]
    pub components: Vec<Component>,
}

fn should_skip_components(components: &[Component]) -> bool {
    components.is_empty()
}

/// The type of a public function.
#[allow(dead_code)]
#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "lowercase")]
pub enum FuncType {
    Function,
    Constructor,
    Receive,
    Fallback,
}

/// The mutability of a public function.
#[allow(dead_code)]
#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "lowercase")]
pub enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

impl From<FunctionAttributes> for Function {
    fn from(attributes: FunctionAttributes) -> Self {
        let name = attributes.name;

        let (name, typ) = match name.as_str() {
            "__init__" => ("".to_string(), FuncType::Constructor),
            _ => (name, FuncType::Function),
        };

        let inputs = attributes
            .params
            .iter()
            .map(|(name, typ)| FuncInput {
                name: name.to_owned(),
                typ: typ.abi_type_name(),
                components: typ
                    .abi_type_components()
                    .iter()
                    .map(|component| Component::from(component.to_owned()))
                    .collect(),
            })
            .collect();

        let return_type = &attributes.return_type;
        let outputs = if return_type.is_empty_tuple() {
            vec![]
        } else {
            let components = if let FixedSize::Struct(struct_) = return_type {
                struct_
                    .abi_type_components()
                    .iter()
                    .map(|component| Component::from(component.to_owned()))
                    .collect()
            } else {
                vec![]
            };

            vec![FuncOutput {
                name: "".to_string(),
                typ: return_type.abi_type_name(),
                components,
            }]
        };

        Self {
            name,
            typ,
            inputs,
            outputs,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::abi::elements::{
        Contract,
        Event,
        EventField,
        FuncInput,
        FuncOutput,
        FuncType,
        Function,
    };

    #[test]
    fn contract_json() {
        let contract = Contract {
            events: vec![Event {
                name: "event_name".to_string(),
                typ: "event".to_string(),
                fields: vec![EventField {
                    name: "input_name".to_string(),
                    typ: "uint256".to_string(),
                    indexed: true,
                }],
                anonymous: false,
            }],
            functions: vec![Function {
                name: "function_name".to_string(),
                typ: FuncType::Function,
                inputs: vec![FuncInput {
                    name: "input_name".to_string(),
                    typ: "address".to_string(),
                    components: vec![],
                }],
                outputs: vec![FuncOutput {
                    name: "output_name".to_string(),
                    typ: "uint256".to_string(),
                    components: vec![],
                }],
            }],
        };

        assert_eq!(
            contract.json(false).unwrap(),
            r#"[
                {
                    "name":"event_name",
                    "type":"event",
                    "inputs":[
                        {
                            "name":"input_name",
                            "type":"uint256",
                            "indexed":true
                        }
                    ],
                    "anonymous":false
                },
                {
                    "name":"function_name",
                    "type":"function",
                    "inputs":[{"name":"input_name","type":"address"}],
                    "outputs":[{"name":"output_name","type":"uint256"}]
                }
            ]"#
            .split_whitespace()
            .collect::<String>(),
        )
    }
}
