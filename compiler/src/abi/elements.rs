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

/// Wrapper around a map of contract names to their ABIs.
#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct ModuleABIs {
    #[serde(flatten)]
    pub contracts: HashMap<String, Contract>,
}

impl Default for ModuleABIs {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleABIs {
    pub fn new() -> Self {
        Self {
            contracts: HashMap::new(),
        }
    }

    /// Serialize the module into a JSON object that maps each contract in the
    /// module to its ABI.
    pub fn json(&self) -> Result<String, CompileError> {
        Ok(serde_json::to_string(self)?)
    }
}

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
    pub fn json(&self) -> Result<String, CompileError> {
        serde_json::to_string(self)
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
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq, Clone)]
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
    Bool,
    Address,
    FixedBytes(usize),
    FixedArray(Box<VarType>, usize),
    String,
}

/// The mutability of a public function.
#[allow(dead_code)]
#[serde(rename_all = "lowercase")]
#[derive(Serialize, Debug, PartialEq, Clone)]
pub enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            VarType::Bool => write!(f, "bool"),
            VarType::Uint256 => write!(f, "uint256"),
            VarType::Address => write!(f, "address"),
            VarType::FixedBytes(size) => write!(f, "bytes{}", size),
            VarType::FixedArray(inner, dim) => write!(f, "{}[{}]", inner, dim),
            VarType::String => write!(f, "string"),
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
        ModuleABIs,
        VarType,
    };
    use std::collections::HashMap;

    #[test]
    fn module_abis_json() {
        let mut contracts = HashMap::new();
        contracts.insert(
            "contract1".to_string(),
            Contract {
                functions: vec![],
                events: vec![],
            },
        );
        contracts.insert(
            "contract2".to_string(),
            Contract {
                functions: vec![],
                events: vec![],
            },
        );

        let json = ModuleABIs { contracts }.json().unwrap();

        assert!(
            json == r#"{"contract1":[],"contract2":[]}"#
                || json == r#"{"contract2":[],"contract1":[]}"#
        )
    }

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
            contract.json().unwrap(),
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
