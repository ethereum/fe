use super::types::AbiType;

use fe_common::utils::keccak;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AbiEvent {
    #[serde(rename = "type")]
    pub ty: &'static str,
    pub name: String,
    pub inputs: Vec<AbiEventField>,
    pub anonymous: bool,
}

impl AbiEvent {
    pub fn new(name: String, fields: Vec<AbiEventField>, anonymous: bool) -> Self {
        Self {
            ty: "event",
            name,
            inputs: fields,
            anonymous,
        }
    }

    pub fn signature(&self) -> AbiEventSignature {
        AbiEventSignature::new(self)
    }
}

pub struct AbiEventSignature {
    sig: String,
}

impl AbiEventSignature {
    pub fn signature(&self) -> &str {
        &self.sig
    }

    pub fn hash_hex(&self) -> String {
        keccak::full(self.sig.as_bytes())
    }

    pub fn hash_raw(&self) -> [u8; 32] {
        keccak::full_as_bytes(self.sig.as_bytes())
    }

    fn new(event: &AbiEvent) -> Self {
        let sig = format!(
            "{}({})",
            event.name,
            event
                .inputs
                .iter()
                .map(|input| input.ty.selector_type_name())
                .collect::<Vec<_>>()
                .join(",")
        );

        Self { sig }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AbiEventField {
    pub name: String,
    #[serde(flatten)]
    pub ty: AbiType,
    pub indexed: bool,
}

impl AbiEventField {
    pub fn new(name: String, ty: impl Into<AbiType>, indexed: bool) -> Self {
        Self {
            name,
            ty: ty.into(),
            indexed,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_test::{assert_ser_tokens, Token};

    fn test_event() -> AbiEvent {
        let i32_ty = AbiType::Int(32);
        let u32_ty = AbiType::UInt(32);
        let field1 = AbiEventField::new("x".into(), i32_ty, true);
        let field2 = AbiEventField::new("y".into(), u32_ty, false);

        AbiEvent::new("MyEvent".into(), vec![field1, field2], false)
    }

    #[test]
    fn serialize_event() {
        let event = test_event();

        assert_ser_tokens(
            &event,
            &[
                Token::Struct {
                    name: "AbiEvent",
                    len: 4,
                },
                Token::Str("type"),
                Token::Str("event"),
                Token::String("name"),
                Token::String("MyEvent"),
                Token::Str("inputs"),
                Token::Seq { len: Some(2) },
                Token::Map { len: None },
                Token::String("name"),
                Token::String("x"),
                Token::String("type"),
                Token::String("int32"),
                Token::Str("indexed"),
                Token::Bool(true),
                Token::MapEnd,
                Token::Map { len: None },
                Token::String("name"),
                Token::String("y"),
                Token::String("type"),
                Token::String("uint32"),
                Token::Str("indexed"),
                Token::Bool(false),
                Token::MapEnd,
                Token::SeqEnd,
                Token::Str("anonymous"),
                Token::Bool(false),
                Token::StructEnd,
            ],
        )
    }

    #[test]
    fn event_signature() {
        let event = test_event();

        let sig = event.signature();
        debug_assert_eq!(sig.signature(), "MyEvent(int32,uint32)");
        debug_assert_eq!(
            sig.hash_hex(),
            "ec835d5150565cb216f72ba07d715e875b0738b1ac3f412e103839e5157b7ee6"
        );
    }
}
