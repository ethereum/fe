use crate::errors::SemanticError;
use fe_parser::ast as fe;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::num::{
    IntErrorKind,
    ParseIntError,
};

use num_bigint::BigInt;

pub fn u256_max() -> BigInt {
    BigInt::from(2).pow(256) - 1
}

pub fn i256_max() -> BigInt {
    BigInt::from(2).pow(255) - 1
}

pub fn i256_min() -> BigInt {
    BigInt::from(-2).pow(255)
}

/// The type has a constant size known to the compiler.
pub trait FeSized {
    /// Constant size of the type.
    fn size(&self) -> usize;
}

/// The size of uint element in the ABI encoding.
///
/// Example: The values inside of a byte array have a padded size of 1 byte and
/// a data size of 1 byte, whereas the values inside of an address array have
/// a padded size of 32 bytes and a data size of 20 bytes. These sizes are
/// needed by our encoding/decoding functions to properly read and write data.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct AbiUintSize {
    ///
    pub data_size: usize,
    pub padded_size: usize,
}

/// The size of an array.
///
/// Can either be statically-sized with a fixed value known by the compiler
/// or a dynamically-sized with the value not being known until runtime.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiArraySize {
    Static { size: usize },
    Dynamic,
}

/// The type of an element in terms of the ABI spec.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiType {
    /// Array elements consist of a dynamically- or statically-sized set of
    /// uints.
    Array {
        inner: Box<AbiType>,
        size: AbiArraySize,
    },
    /// All elements are encoded as a uint or set of uints.
    Uint { size: AbiUintSize },
}

/// Data can be decoded from memory or calldata.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiDecodeLocation {
    Calldata,
    Memory,
}

/// Information relevant to ABI encoding.
pub trait AbiEncoding {
    /// Name of the type as it appears in the Json ABI.
    fn abi_name(&self) -> String;

    /// A name that uses identifier-friendly characters and avoids collisions.
    /// This is used internally for generating encoding/decoding functions.
    ///
    /// Examples:
    /// - `address[100]` cannot be used in an identifier, so we use the safe
    /// name address100 instead.
    /// - All strings are represented as "string" in the Json ABI. This is
    /// is not compatible with our internal ABI functions because our decoding
    /// function will need to account for maximum sizes. Therefore, we can
    /// not use the same decoding function for two `string`s with different
    /// max sizes and we need to differentiate the two.
    fn abi_safe_name(&self) -> String;

    /// The ABI type of a Fe type.
    fn abi_type(&self) -> AbiType;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
    Tuple(Tuple),
    String(FeString),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
    Tuple(Tuple),
    String(FeString),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum Base {
    Numeric(Integer),
    Bool,
    Byte,
    Address,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum Integer {
    U256,
    U128,
    U64,
    U32,
    U16,
    U8,
    I256,
    I128,
    I64,
    I32,
    I16,
    I8,
}

pub const U256: Base = Base::Numeric(Integer::U256);

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct Array {
    pub dimension: usize,
    pub inner: Base,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub key: Base,
    pub value: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct Tuple {
    pub items: Vec<Base>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct FeString {
    pub max_size: usize,
}

impl TryFrom<&str> for FeString {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if !value.starts_with("string") {
            return Err("Value must start with 'string'".to_string());
        }

        let max_size = value[6..].parse::<u32>().map_err(|err| err.to_string())? as usize;

        Ok(FeString { max_size })
    }
}

impl Integer {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Integer::I256
                | Integer::I128
                | Integer::I64
                | Integer::I32
                | Integer::I16
                | Integer::I8
        )
    }

    pub fn fits(&self, num: &str) -> bool {
        let radix = 10;

        fn handle_parse_error<T>(result: Result<T, ParseIntError>) -> bool {
            if let Err(error) = result {
                return match error.kind() {
                    IntErrorKind::PosOverflow => false,
                    IntErrorKind::NegOverflow => false,
                    // If we try to parse a negative value for an unsigned type
                    IntErrorKind::InvalidDigit => false,
                    // We don't expect this but it would be tragic if we would map this to `false`
                    // incase it happens because it would mean we sweep a bug under the rug.
                    other => panic!("Unexpected ParseIntError: {:?}", other),
                };
            }

            true
        }

        match self {
            Integer::U8 => handle_parse_error(u8::from_str_radix(num, radix)),
            Integer::U16 => handle_parse_error(u16::from_str_radix(num, radix)),
            Integer::U32 => handle_parse_error(u32::from_str_radix(num, radix)),
            Integer::U64 => handle_parse_error(u64::from_str_radix(num, radix)),
            Integer::U128 => handle_parse_error(u128::from_str_radix(num, radix)),
            Integer::U256 => BigInt::parse_bytes(num.as_bytes(), radix)
                .map_or(false, |val| val >= BigInt::from(0) && val <= u256_max()),
            Integer::I8 => handle_parse_error(i8::from_str_radix(num, radix)),
            Integer::I16 => handle_parse_error(i16::from_str_radix(num, radix)),
            Integer::I32 => handle_parse_error(i32::from_str_radix(num, radix)),
            Integer::I64 => handle_parse_error(i64::from_str_radix(num, radix)),
            Integer::I128 => handle_parse_error(i128::from_str_radix(num, radix)),
            Integer::I256 => BigInt::parse_bytes(num.as_bytes(), radix)
                .map_or(false, |val| val >= i256_min() && val <= i256_max()),
        }
    }
}

impl Type {
    /// Returns true if the type is a tuple with 0 elements.
    pub fn is_empty_tuple(&self) -> bool {
        if let Type::Tuple(tuple) = &self {
            return tuple.is_empty();
        }
        false
    }

    pub fn is_signed_integer(&self) -> bool {
        if let Type::Base(Base::Numeric(integer)) = &self {
            return integer.is_signed();
        }
        false
    }
}

impl From<FixedSize> for Type {
    fn from(value: FixedSize) -> Self {
        match value {
            FixedSize::Array(array) => Type::Array(array),
            FixedSize::Base(base) => Type::Base(base),
            FixedSize::Tuple(tuple) => Type::Tuple(tuple),
            FixedSize::String(string) => Type::String(string),
        }
    }
}

impl FeSized for FixedSize {
    fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size(),
            FixedSize::Tuple(tuple) => tuple.size(),
            FixedSize::String(string) => string.size(),
        }
    }
}

impl FeSized for Integer {
    fn size(&self) -> usize {
        match self {
            Integer::U8 => 1,
            Integer::U16 => 2,
            Integer::U32 => 4,
            Integer::U64 => 8,
            Integer::U128 => 16,
            Integer::U256 => 32,
            Integer::I8 => 1,
            Integer::I16 => 2,
            Integer::I32 => 4,
            Integer::I64 => 8,
            Integer::I128 => 16,
            Integer::I256 => 32,
        }
    }
}

impl AbiEncoding for FixedSize {
    fn abi_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_name(),
            FixedSize::Base(base) => base.abi_name(),
            FixedSize::Tuple(tuple) => tuple.abi_name(),
            FixedSize::String(string) => string.abi_name(),
        }
    }

    fn abi_safe_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_safe_name(),
            FixedSize::Base(base) => base.abi_safe_name(),
            FixedSize::Tuple(tuple) => tuple.abi_safe_name(),
            FixedSize::String(string) => string.abi_safe_name(),
        }
    }

    fn abi_type(&self) -> AbiType {
        match self {
            FixedSize::Base(base) => base.abi_type(),
            FixedSize::Array(array) => array.abi_type(),
            FixedSize::Tuple(tuple) => tuple.abi_type(),
            FixedSize::String(string) => string.abi_type(),
        }
    }
}

impl FixedSize {
    pub fn empty_tuple() -> Self {
        FixedSize::Tuple(Tuple::empty())
    }

    pub fn is_empty_tuple(&self) -> bool {
        if let FixedSize::Tuple(tuple) = self {
            return tuple.is_empty();
        }

        false
    }
}

impl TryFrom<Type> for FixedSize {
    type Error = SemanticError;

    fn try_from(value: Type) -> Result<Self, SemanticError> {
        match value {
            Type::Array(array) => Ok(FixedSize::Array(array)),
            Type::Base(base) => Ok(FixedSize::Base(base)),
            Type::Tuple(tuple) => Ok(FixedSize::Tuple(tuple)),
            Type::String(string) => Ok(FixedSize::String(string)),
            Type::Map(_) => Err(SemanticError::type_error()),
        }
    }
}

impl FeSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::Numeric(integer) => integer.size(),
            Base::Bool => 1,
            Base::Byte => 1,
            Base::Address => 20,
        }
    }
}

impl AbiEncoding for Base {
    fn abi_name(&self) -> String {
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
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
        }
    }

    fn abi_safe_name(&self) -> String {
        self.abi_name()
    }

    fn abi_type(&self) -> AbiType {
        match self {
            Base::Bool => AbiType::Uint {
                size: AbiUintSize {
                    data_size: 1,
                    padded_size: 32,
                },
            },
            Base::Numeric(size) => match size {
                Integer::U256 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 32,
                        padded_size: 32,
                    },
                },
                Integer::U128 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 16,
                        padded_size: 32,
                    },
                },
                Integer::U64 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 8,
                        padded_size: 32,
                    },
                },
                Integer::U32 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 4,
                        padded_size: 32,
                    },
                },
                Integer::U16 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 2,
                        padded_size: 32,
                    },
                },
                Integer::U8 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 1,
                        padded_size: 32,
                    },
                },
                Integer::I256 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 32,
                        padded_size: 32,
                    },
                },
                Integer::I128 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 16,
                        padded_size: 32,
                    },
                },
                Integer::I64 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 8,
                        padded_size: 32,
                    },
                },
                Integer::I32 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 4,
                        padded_size: 32,
                    },
                },
                Integer::I16 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 2,
                        padded_size: 32,
                    },
                },
                Integer::I8 => AbiType::Uint {
                    size: AbiUintSize {
                        data_size: 1,
                        padded_size: 32,
                    },
                },
            },
            Base::Address => AbiType::Uint {
                size: AbiUintSize {
                    data_size: 20,
                    padded_size: 32,
                },
            },
            Base::Byte => AbiType::Uint {
                size: AbiUintSize {
                    data_size: 1,
                    padded_size: 1,
                },
            },
        }
    }
}

impl FeSized for Array {
    fn size(&self) -> usize {
        self.dimension * self.inner.size()
    }
}

impl AbiEncoding for Array {
    fn abi_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.dimension);
        }

        format!("{}[{}]", self.inner.abi_name(), self.dimension)
    }

    fn abi_safe_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.dimension);
        }

        format!("{}{}", self.inner.abi_name(), self.dimension)
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(self.inner.abi_type()),
            size: AbiArraySize::Static {
                size: self.dimension,
            },
        }
    }
}

impl Tuple {
    pub fn empty() -> Tuple {
        Tuple { items: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }
}

impl From<Tuple> for FixedSize {
    fn from(value: Tuple) -> Self {
        FixedSize::Tuple(value)
    }
}

impl FeSized for Tuple {
    fn size(&self) -> usize {
        self.items.iter().map(|typ| typ.size()).sum()
    }
}

impl AbiEncoding for Tuple {
    fn abi_name(&self) -> String {
        unimplemented!();
    }

    fn abi_safe_name(&self) -> String {
        unimplemented!();
    }

    fn abi_type(&self) -> AbiType {
        unimplemented!();
    }
}

impl FeSized for FeString {
    fn size(&self) -> usize {
        self.max_size + 32
    }
}

impl AbiEncoding for FeString {
    fn abi_name(&self) -> String {
        "string".to_string()
    }

    fn abi_safe_name(&self) -> String {
        format!("string{}", self.max_size)
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(AbiType::Uint {
                size: AbiUintSize {
                    data_size: 1,
                    padded_size: 1,
                },
            }),
            size: AbiArraySize::Dynamic,
        }
    }
}

pub fn type_desc_fixed_size(
    defs: &HashMap<String, Type>,
    typ: &fe::TypeDesc,
) -> Result<FixedSize, SemanticError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(FixedSize::Base(base)),
        Type::Array(array) => Ok(FixedSize::Array(array)),
        Type::Tuple(tuple) => Ok(FixedSize::Tuple(tuple)),
        Type::String(string) => Ok(FixedSize::String(string)),
        Type::Map(_) => Err(SemanticError::type_error()),
    }
}

pub fn type_desc_base(
    defs: &HashMap<String, Type>,
    typ: &fe::TypeDesc,
) -> Result<Base, SemanticError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        _ => Err(SemanticError::type_error()),
    }
}

pub fn type_desc(defs: &HashMap<String, Type>, typ: &fe::TypeDesc) -> Result<Type, SemanticError> {
    match typ {
        fe::TypeDesc::Base { base: "u256" } => Ok(Type::Base(U256)),
        fe::TypeDesc::Base { base: "u128" } => Ok(Type::Base(Base::Numeric(Integer::U128))),
        fe::TypeDesc::Base { base: "u64" } => Ok(Type::Base(Base::Numeric(Integer::U64))),
        fe::TypeDesc::Base { base: "u32" } => Ok(Type::Base(Base::Numeric(Integer::U32))),
        fe::TypeDesc::Base { base: "u16" } => Ok(Type::Base(Base::Numeric(Integer::U16))),
        fe::TypeDesc::Base { base: "u8" } => Ok(Type::Base(Base::Numeric(Integer::U8))),
        fe::TypeDesc::Base { base: "i256" } => Ok(Type::Base(Base::Numeric(Integer::I256))),
        fe::TypeDesc::Base { base: "i128" } => Ok(Type::Base(Base::Numeric(Integer::I128))),
        fe::TypeDesc::Base { base: "i64" } => Ok(Type::Base(Base::Numeric(Integer::I64))),
        fe::TypeDesc::Base { base: "i32" } => Ok(Type::Base(Base::Numeric(Integer::I32))),
        fe::TypeDesc::Base { base: "i16" } => Ok(Type::Base(Base::Numeric(Integer::I16))),
        fe::TypeDesc::Base { base: "i8" } => Ok(Type::Base(Base::Numeric(Integer::I8))),
        fe::TypeDesc::Base { base: "bool" } => Ok(Type::Base(Base::Bool)),
        fe::TypeDesc::Base { base: "bytes" } => Ok(Type::Base(Base::Byte)),
        fe::TypeDesc::Base { base: "address" } => Ok(Type::Base(Base::Address)),
        fe::TypeDesc::Base { base } if base.starts_with("string") => Ok(Type::String(
            TryFrom::try_from(*base).map_err(|_| SemanticError::type_error())?,
        )),
        fe::TypeDesc::Base { base } => {
            if let Some(typ) = defs.get(base.to_owned()) {
                return Ok(typ.clone());
            }

            Err(SemanticError::undefined_value())
        }
        fe::TypeDesc::Array { typ, dimension } => Ok(Type::Array(Array {
            inner: type_desc_base(defs, &typ.node)?,
            dimension: *dimension,
        })),
        fe::TypeDesc::Map { from, to } => Ok(Type::Map(Map {
            key: type_desc_base(defs, &from.node)?,
            value: Box::new(type_desc(defs, &to.node)?),
        })),
        fe::TypeDesc::Tuple { items } => Ok(Type::Tuple(Tuple {
            items: items
                .iter()
                .map(|typ| type_desc_base(defs, &typ.node))
                .collect::<Result<_, _>>()?,
        })),
    }
}
