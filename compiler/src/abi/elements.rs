use crate::errors::CompileError;
use core::fmt;
use serde::export::fmt::Error;
use serde::export::Formatter;
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
use std::collections::HashMap;

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct ModuleABIs(pub HashMap<String, Contract>);

impl ModuleABIs {
    /// Serialize the module into a JSON object that maps each contract in the module to its ABI.
    pub fn json(&self) -> Result<String, CompileError> {
        serde_json::to_string(self)
            .map_err(|_| CompileError::static_str("Unable to serialize contract to json"))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Contract {
    pub events: Vec<Event>,
    pub functions: Vec<Function>,
}

impl Contract {
    /// Serialize the contract into a valid JSON ABI.
    pub fn json(&self) -> Result<String, CompileError> {
        serde_json::to_string(self)
            .map_err(|_| CompileError::static_str("Unable to serialize contract to json"))
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

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Event {
    pub name: String,
    // `typ` is always set to "event"
    #[serde(rename = "type")]
    pub typ: String,
    pub inputs: Vec<EventField>,
    pub anonymous: bool,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct EventField {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: VariableType,
    pub indexed: bool,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: FunctionType,
    pub inputs: Vec<FuncInput>,
    pub outputs: Vec<FuncOutput>,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncInput {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: VariableType,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct FuncOutput {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: VariableType,
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

impl Serialize for VariableType {
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
        Contract, Event, EventField, FuncInput, FuncOutput, Function, FunctionType, ModuleABIs,
        VariableType,
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

        let json = ModuleABIs(contracts).json().unwrap();

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
                inputs: vec![EventField {
                    name: "input_name".to_string(),
                    typ: VariableType::Uint256,
                    indexed: true,
                }],
                anonymous: false,
            }],
            functions: vec![Function {
                name: "function_name".to_string(),
                typ: FunctionType::Function,
                inputs: vec![FuncInput {
                    name: "input_name".to_string(),
                    typ: VariableType::Address,
                }],
                outputs: vec![FuncOutput {
                    name: "output_name".to_string(),
                    typ: VariableType::Uint256,
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
            serde_json::to_string(&VariableType::FixedBytes(100)).unwrap(),
            r#""bytes100""#
        )
    }

    #[test]
    fn fixed_array() {
        assert_eq!(
            serde_json::to_string(&VariableType::FixedArray(
                Box::new(VariableType::Address),
                42
            ))
            .unwrap(),
            r#""address[42]""#
        )
    }
}
