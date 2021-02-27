use crate::errors::CompileError;
use core::fmt;
use serde::export::fmt::Error;
use serde::export::Formatter;
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
    // FIXME: This attribute should be added when serialized instead of being defined in the
    // struct.
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
    pub typ: VarType,
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

/// A single function input.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncInput {
    /// The input's name.
    pub name: String,
    /// The input's type.
    #[serde(rename = "type")]
    pub typ: VarType,
}

/// A single function output.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncOutput {
    /// The output's name.
    pub name: String,
    /// The output's type.
    #[serde(rename = "type")]
    pub typ: VarType,
}

// FIXME: This should be removed in favor of separate structs for each function
// type.
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

/// The type of an event field or function input or output.
#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Uint256,
    Uint128,
    Uint64,
    Uint32,
    Uint16,
    Uint8,
    Int256,
    Int128,
    Int64,
    Int32,
    Int16,
    Int8,
    Bool,
    Address,
    FixedBytes(usize),
    FixedArray(Box<VarType>, usize),
    String,
    Tuple(Vec<VarType>),
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

impl fmt::Display for VarType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            VarType::Bool => write!(formatter, "bool"),
            VarType::Uint256 => write!(formatter, "uint256"),
            VarType::Uint128 => write!(formatter, "uint128"),
            VarType::Uint64 => write!(formatter, "uint64"),
            VarType::Uint32 => write!(formatter, "uint32"),
            VarType::Uint16 => write!(formatter, "uint16"),
            VarType::Uint8 => write!(formatter, "uint8"),
            VarType::Int256 => write!(formatter, "int256"),
            VarType::Int128 => write!(formatter, "int128"),
            VarType::Int64 => write!(formatter, "int64"),
            VarType::Int32 => write!(formatter, "int32"),
            VarType::Int16 => write!(formatter, "int16"),
            VarType::Int8 => write!(formatter, "int8"),
            VarType::Address => write!(formatter, "address"),
            VarType::FixedBytes(size) => write!(formatter, "bytes{}", size),
            VarType::FixedArray(inner, dim) => write!(formatter, "{}[{}]", inner, dim),
            VarType::String => write!(formatter, "string"),
            VarType::Tuple(items) => {
                let items = items
                    .iter()
                    .map(VarType::to_string)
                    .collect::<Vec<String>>()
                    .join(",");
                write!(formatter, "({})", items)
            }
        }
    }
}

impl Serialize for VarType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
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
        VarType,
    };

    #[test]
    fn contract_json() {
        let contract = Contract {
            events: vec![Event {
                name: "event_name".to_string(),
                typ: "event".to_string(),
                fields: vec![EventField {
                    name: "input_name".to_string(),
                    typ: VarType::Uint256,
                    indexed: true,
                }],
                anonymous: false,
            }],
            functions: vec![Function {
                name: "function_name".to_string(),
                typ: FuncType::Function,
                inputs: vec![FuncInput {
                    name: "input_name".to_string(),
                    typ: VarType::Address,
                }],
                outputs: vec![FuncOutput {
                    name: "output_name".to_string(),
                    typ: VarType::Uint256,
                }],
            }],
        };

        assert_eq!(
            contract.json(false).unwrap(),
            r#"[
                {
                    "name":"event_name",
                    "type":"event",
                    "inputs":[{"name":"input_name","type":"uint256","indexed":true}],
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

    #[test]
    fn fixed_bytes() {
        assert_eq!(
            serde_json::to_string(&VarType::FixedBytes(100)).unwrap(),
            r#""bytes100""#
        )
    }

    #[test]
    fn fixed_array() {
        assert_eq!(
            serde_json::to_string(&VarType::FixedArray(Box::new(VarType::Address), 42)).unwrap(),
            r#""address[42]""#
        )
    }
}
