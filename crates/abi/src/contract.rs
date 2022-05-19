use super::{event::AbiEvent, function::AbiFunction};

use serde::{ser::SerializeSeq, Serialize, Serializer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbiContract {
    /// Public functions in the contract.
    funcs: Vec<AbiFunction>,

    /// Events emitted from the contract.
    events: Vec<AbiEvent>,
}

impl Serialize for AbiContract {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut seq = s.serialize_seq(Some(self.funcs.len() + self.events.len()))?;
        for func in &self.funcs {
            seq.serialize_element(func)?;
        }

        for event in &self.events {
            seq.serialize_element(event)?;
        }

        seq.end()
    }
}

impl AbiContract {
    pub fn new(funcs: Vec<AbiFunction>, events: Vec<AbiEvent>) -> Self {
        Self { funcs, events }
    }
}
