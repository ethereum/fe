use crate::errors::AbiError;
use fe_analyzer::namespace::types::{Array, Base, FeString, Integer, Struct, Tuple, Type};
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
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
    pub fn json(&self, prettify: bool) -> Result<String, AbiError> {
        match prettify {
            true => serde_json::to_string_pretty(self),
            false => serde_json::to_string(self),
        }
        .map_err(|_| AbiError::SerializationFailed)
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

/// Single component of a tuple.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct AbiComponent {
    pub name: String,
    pub typ: String,
    /// The subcomponents of the component.
    pub components: Vec<AbiComponent>,
}

/// Information relevant to ABI encoding.
pub trait JsonAbi {
    /// Name of the type as it appears in the Json ABI.
    fn abi_json_name(&self) -> String;
}

impl JsonAbi for Type {
    fn abi_json_name(&self) -> String {
        match self {
            Type::Array(array) => array.abi_json_name(),
            Type::Base(base) => base.abi_json_name(),
            Type::Tuple(tuple) => tuple.abi_json_name(),
            Type::String(string) => string.abi_json_name(),
            Type::Contract(_) => "address".to_string(),
            Type::Struct(val) => val.abi_json_name(),
            _ => panic!("not abi encodable"),
        }
    }
}

impl JsonAbi for Base {
    fn abi_json_name(&self) -> String {
        match self {
            Base::Numeric(Integer::U256) => "uint256".to_string(),
            Base::Numeric(Integer::U128) => "uint128".to_string(),
            Base::Numeric(Integer::U64) => "uint64".to_string(),
            Base::Numeric(Integer::U32) => "uint32".to_string(),
            Base::Numeric(Integer::U16) => "uint16".to_string(),
            Base::Numeric(Integer::U8) => "uint8".to_string(),
            Base::Numeric(Integer::I256) => "int256".to_string(),
            Base::Numeric(Integer::I128) => "int128".to_string(),
            Base::Numeric(Integer::I64) => "int64".to_string(),
            Base::Numeric(Integer::I32) => "int32".to_string(),
            Base::Numeric(Integer::I16) => "int16".to_string(),
            Base::Numeric(Integer::I8) => "int8".to_string(),
            Base::Address => "address".to_string(),
            Base::Bool => "bool".to_string(),
            Base::Unit => panic!("unit type is not abi encodable"),
        }
    }
}

impl JsonAbi for Array {
    fn abi_json_name(&self) -> String {
        if self.inner == Base::Numeric(Integer::U8) {
            "bytes".to_string()
        } else {
            format!("{}[{}]", self.inner.abi_json_name(), self.size)
        }
    }
}

impl JsonAbi for Struct {
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }
}

impl JsonAbi for Tuple {
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }
}

impl JsonAbi for FeString {
    fn abi_json_name(&self) -> String {
        "string".to_string()
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
    /// Components of a tuple. This field is excluded if there are no
    /// components.
    #[serde(skip_serializing_if = "should_skip_components")]
    pub components: Vec<Component>,
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
    pub name: String,
    #[serde(rename = "type")]
    pub typ: String,
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

#[cfg(test)]
mod tests {
    use crate::elements::{Contract, Event, EventField, FuncInput, FuncOutput, FuncType, Function};

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
                    components: vec![],
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
