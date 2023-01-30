use serde::{ser::SerializeMap, Serialize, Serializer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbiType {
    UInt(usize),
    Int(usize),
    Address,
    Bool,
    Function,
    Array { elem_ty: Box<AbiType>, len: usize },
    Tuple(Vec<AbiTupleField>),
    Bytes,
    String,
}

impl AbiType {
    pub fn selector_type_name(&self) -> String {
        match self {
            Self::UInt(bits) => format!("uint{bits}"),
            Self::Int(bits) => format!("int{bits}"),
            Self::Address => "address".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Function => "function".to_string(),
            Self::Array { elem_ty, len } => {
                if elem_ty.as_ref() == &AbiType::UInt(8) {
                    "bytes".to_string()
                } else {
                    format!("{}[{}]", elem_ty.selector_type_name(), len)
                }
            }
            Self::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(|component| component.ty.selector_type_name())
                    .collect::<Vec<_>>()
                    .join(",")
            ),

            Self::Bytes => "bytes".to_string(),
            Self::String => "string".to_string(),
        }
    }

    pub fn abi_type_name(&self) -> String {
        match self {
            Self::Tuple(_) => "tuple".to_string(),
            Self::Array { elem_ty, len } => {
                if elem_ty.as_ref() == &AbiType::UInt(8) {
                    "bytes".to_string()
                } else {
                    format!("{}[{}]", elem_ty.abi_type_name(), len)
                }
            }
            _ => self.selector_type_name(),
        }
    }

    pub fn header_size(&self) -> usize {
        match self {
            Self::UInt(_) | Self::Int(_) | Self::Address | Self::Bool | Self::Function => 32,

            Self::Array { elem_ty, len } if elem_ty.is_static() => elem_ty.header_size() * len,
            Self::Array { .. } => 32,

            Self::Tuple(fields) if self.is_static() => fields
                .iter()
                .fold(0, |acc, field| field.ty.header_size() + acc),
            Self::Tuple(_) => 32,

            Self::Bytes | Self::String => 32,
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches! {
            self,
            Self::UInt(_) | Self::Int(_) | Self::Address | Self::Bool
        }
    }

    pub fn is_bytes(&self) -> bool {
        matches! {
            self,
            Self::Bytes,
        }
    }

    pub fn is_string(&self) -> bool {
        matches! {
            self,
            Self::String
        }
    }

    pub fn is_static(&self) -> bool {
        match self {
            Self::UInt(_) | Self::Int(_) | Self::Address | Self::Bool | Self::Function => true,
            Self::Array { elem_ty, .. } => elem_ty.is_static(),
            Self::Tuple(fields) => fields.iter().all(|field| field.ty.is_static()),
            Self::Bytes | Self::String => false,
        }
    }

    /// Returns bytes size of the encoded type if the type is static.
    pub fn size(&self) -> Option<usize> {
        match self {
            Self::UInt(_) | Self::Int(_) | Self::Address | Self::Bool => Some(32),
            Self::Function => Some(24),
            Self::Array { elem_ty, len } => Some(elem_ty.size()? * len),
            Self::Tuple(fields) => {
                let mut size = 0;
                for field in fields.iter() {
                    size += field.ty.size()?;
                }
                Some(size)
            }

            Self::Bytes | Self::String => None,
        }
    }

    fn serialize_component<S: SerializeMap>(&self, s: &mut S) -> Result<(), S::Error> {
        match self {
            Self::Tuple(entry) => s.serialize_entry("components", entry),
            Self::Array { elem_ty, .. } => elem_ty.serialize_component(s),
            _ => Ok(()),
        }
    }
}

impl Serialize for AbiType {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut map = s.serialize_map(None)?;
        let type_name = self.abi_type_name();

        map.serialize_entry("type", &type_name)?;

        self.serialize_component(&mut map)?;
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AbiTupleField {
    name: String,
    #[serde(flatten)]
    ty: AbiType,
}

impl AbiTupleField {
    pub fn new(name: String, ty: impl Into<AbiType>) -> Self {
        Self {
            name,
            ty: ty.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_test::{assert_ser_tokens, Token};

    #[test]
    fn primitive() {
        let u32_ty = AbiType::UInt(32);
        assert_ser_tokens(
            &u32_ty,
            &[
                Token::Map { len: None },
                Token::String("type"),
                Token::String("uint32"),
                Token::MapEnd,
            ],
        )
    }

    #[test]
    fn primitive_array() {
        let i32_ty = AbiType::Int(32);
        let array_i32 = AbiType::Array {
            elem_ty: i32_ty.into(),
            len: 10,
        };

        assert_ser_tokens(
            &array_i32,
            &[
                Token::Map { len: None },
                Token::String("type"),
                Token::String("int32[10]"),
                Token::MapEnd,
            ],
        )
    }

    #[test]
    fn tuple_array() {
        let u16_ty = AbiType::UInt(16);
        let bool_ty = AbiType::Bool;
        let array_bool = AbiType::Array {
            elem_ty: bool_ty.into(),
            len: 16,
        };

        let field1 = AbiTupleField::new("field1".into(), u16_ty);
        let field2 = AbiTupleField::new("field2".into(), array_bool);
        let tuple_ty = AbiType::Tuple(vec![field1, field2]);

        let tuple_array_ty = AbiType::Array {
            elem_ty: tuple_ty.into(),
            len: 16,
        };

        assert_ser_tokens(
            &tuple_array_ty,
            &[
                Token::Map { len: None },
                Token::String("type"),
                Token::String("tuple[16]"),
                Token::String("components"),
                Token::Seq { len: Some(2) },
                // Field1.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field1"),
                Token::String("type"),
                Token::String("uint16"),
                Token::MapEnd,
                // Field2.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field2"),
                Token::String("type"),
                Token::String("bool[16]"),
                Token::MapEnd,
                Token::SeqEnd,
                Token::MapEnd,
            ],
        )
    }

    #[test]
    fn simple_tuple() {
        let u16_ty = AbiType::UInt(16);
        let bool_ty = AbiType::Bool;
        let bool_array_ty = AbiType::Array {
            elem_ty: bool_ty.into(),
            len: 16,
        };

        let field1 = AbiTupleField::new("field1".into(), u16_ty);
        let field2 = AbiTupleField::new("field2".into(), bool_array_ty);
        let tuple_ty = AbiType::Tuple(vec![field1, field2]);

        assert_ser_tokens(
            &tuple_ty,
            &[
                Token::Map { len: None },
                Token::String("type"),
                Token::String("tuple"),
                Token::String("components"),
                Token::Seq { len: Some(2) },
                // Field1.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field1"),
                Token::String("type"),
                Token::String("uint16"),
                Token::MapEnd,
                // Field2.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field2"),
                Token::String("type"),
                Token::String("bool[16]"),
                Token::MapEnd,
                Token::SeqEnd,
                Token::MapEnd,
            ],
        )
    }

    #[test]
    fn complex_tuple() {
        let u16_ty = AbiType::UInt(16);
        let bool_ty = AbiType::Bool;

        let inner_field1 = AbiTupleField::new("inner_field1".into(), u16_ty);
        let inner_field2 = AbiTupleField::new("inner_field2".into(), bool_ty);
        let inner_tuple_ty = AbiType::Tuple(vec![inner_field1, inner_field2]);

        let inner_tuple_array_ty = AbiType::Array {
            elem_ty: inner_tuple_ty.clone().into(),
            len: 16,
        };

        let outer_field1 = AbiTupleField::new("outer_field1".into(), inner_tuple_array_ty);
        let outer_field2 = AbiTupleField::new("outer_field2".into(), inner_tuple_ty);
        let outer_tuple_ty = AbiType::Tuple(vec![outer_field1, outer_field2]);

        assert_ser_tokens(
            &outer_tuple_ty,
            &[
                // Outer tuple start.
                Token::Map { len: None },
                Token::String("type"),
                Token::String("tuple"),
                // Outer tuple components start.
                Token::String("components"),
                Token::Seq { len: Some(2) },
                // Outer field1 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("outer_field1"),
                Token::String("type"),
                Token::String("tuple[16]"),
                Token::String("components"),
                Token::Seq { len: Some(2) },
                // Inner field1 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("inner_field1"),
                Token::String("type"),
                Token::String("uint16"),
                Token::MapEnd,
                // Inner field1 end.
                // Inner field2 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("inner_field2"),
                Token::String("type"),
                Token::String("bool"),
                Token::MapEnd,
                // Inner field2 end.
                Token::SeqEnd,
                Token::MapEnd,
                // Outer field1 end.
                // Outer field2 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("outer_field2"),
                Token::String("type"),
                Token::String("tuple"),
                Token::String("components"),
                Token::Seq { len: Some(2) },
                // Inner field1 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("inner_field1"),
                Token::String("type"),
                Token::String("uint16"),
                Token::MapEnd,
                // Inner field1 end.
                // Inner field2 start.
                Token::Map { len: None },
                Token::String("name"),
                Token::String("inner_field2"),
                Token::String("type"),
                Token::String("bool"),
                Token::MapEnd,
                // Inner field2 end.
                Token::SeqEnd,
                Token::MapEnd,
                // Outer field2 end.
                Token::SeqEnd,
                // Outer tuple components end.
                Token::MapEnd,
            ],
        )
    }
}
