use crate::errors::SemanticError;
use fe_parser::ast as fe;
use std::collections::{
    btree_map::Entry,
    BTreeMap,
    HashMap,
};
use std::convert::TryFrom;

use crate::FunctionAttributes;
use num_bigint::BigInt;
use strum::IntoStaticStr;

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
    Tuple {
        elems: Vec<AbiType>,
    },
    /// All elements are encoded as a uint or set of uints.
    Uint {
        size: AbiUintSize,
    },
}

/// Data can be decoded from memory or calldata.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiDecodeLocation {
    Calldata,
    Memory,
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
pub trait AbiEncoding {
    /// Name of the type as it appears in the Json ABI.
    fn abi_type_name(&self) -> String;

    /// The components of an ABI tuple.
    fn abi_type_components(&self) -> Vec<AbiComponent>;

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
    Contract(Contract),
    Struct(Struct),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
    Tuple(Tuple),
    String(FeString),
    Contract(Contract),
    Struct(Struct),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum Base {
    Numeric(Integer),
    Bool,
    Byte,
    Address,
}

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Ord, Eq, IntoStaticStr)]
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
    pub size: usize,
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
pub struct Struct {
    pub name: String,
    fields: BTreeMap<String, FixedSize>,
    order: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct FeString {
    pub max_size: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct Contract {
    pub name: String,
    pub functions: Vec<FunctionAttributes>,
}

impl Struct {
    pub fn new(name: &str) -> Struct {
        Struct {
            name: name.to_string(),
            fields: BTreeMap::new(),
            order: vec![],
        }
    }

    /// Return `true` if the struct has any fields, otherwise return `false`
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Add a field to the struct
    pub fn add_field(&mut self, name: &str, value: &FixedSize) -> Result<(), SemanticError> {
        match self.fields.entry(name.to_owned()) {
            Entry::Occupied(_) => Err(SemanticError::already_defined()),
            Entry::Vacant(entry) => {
                entry.insert(value.clone());
                self.order.push(name.to_string());
                Ok(())
            }
        }
    }

    /// Return the type of the given field name
    pub fn get_field_type(&self, name: &str) -> Option<&FixedSize> {
        self.fields.get(name)
    }

    /// Return the index of the given field name
    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.order.iter().position(|field| field == name)
    }

    /// Return a vector of field types
    pub fn get_field_types(&self) -> Vec<FixedSize> {
        self.order
            .iter()
            .map(|name| {
                self.get_field_type(name)
                    .expect("no entry for field name")
                    .to_owned()
            })
            .collect()
    }

    /// Return a vector of field names
    pub fn get_field_names(&self) -> Vec<String> {
        self.order.clone()
    }

    pub fn get_num_fields(&self) -> usize {
        self.order.len()
    }
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
    /// Returns `true` if the integer is signed, otherwise `false`
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

    /// Returns `true` if the integer is at least the same size (or larger) than
    /// `other`
    pub fn can_hold(&self, other: &Integer) -> bool {
        self.size() >= other.size()
    }

    /// Returns `true` if `num` represents a number that fits the type
    pub fn fits(&self, num: &str) -> bool {
        use lexical_core::{
            parse,
            ErrorCode,
            FromLexical,
        };
        fn check_fit<T: FromLexical>(num: &str) -> bool {
            if let Err(err) = parse::<T>(num.as_bytes()) {
                match err.code {
                    ErrorCode::Overflow => false,
                    ErrorCode::Underflow => false,
                    // If we try to parse a negative value for an unsigned type
                    ErrorCode::InvalidDigit => false,
                    // We don't expect this but it would be tragic if we would map this to `false`
                    // incase it happens because it would mean we sweep a bug under the rug.
                    other => panic!("Unexpected ParseIntError: {:?}", other),
                }
            } else {
                true
            }
        }

        // We reject octal number literals.
        if num.len() > 1 && num.starts_with('0') {
            return false;
        }

        let radix = 10;
        match self {
            Integer::U8 => check_fit::<u8>(num),
            Integer::U16 => check_fit::<u16>(num),
            Integer::U32 => check_fit::<u32>(num),
            Integer::U64 => check_fit::<u64>(num),
            Integer::U128 => check_fit::<u128>(num),
            Integer::U256 => BigInt::parse_bytes(num.as_bytes(), radix)
                .map_or(false, |val| val >= BigInt::from(0) && val <= u256_max()),
            Integer::I8 => check_fit::<i8>(num),
            Integer::I16 => check_fit::<i16>(num),
            Integer::I32 => check_fit::<i32>(num),
            Integer::I64 => check_fit::<i64>(num),
            Integer::I128 => check_fit::<i128>(num),
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
            FixedSize::Contract(contract) => Type::Contract(contract),
            FixedSize::Struct(val) => Type::Struct(val),
        }
    }
}

impl From<Base> for Type {
    fn from(value: Base) -> Self {
        Type::Base(value)
    }
}

impl FeSized for FixedSize {
    fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size(),
            FixedSize::Tuple(tuple) => tuple.size(),
            FixedSize::String(string) => string.size(),
            FixedSize::Contract(contract) => contract.size(),
            FixedSize::Struct(val) => val.size(),
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
    fn abi_type_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_type_name(),
            FixedSize::Base(base) => base.abi_type_name(),
            FixedSize::Tuple(tuple) => tuple.abi_type_name(),
            FixedSize::String(string) => string.abi_type_name(),
            FixedSize::Contract(contract) => contract.abi_type_name(),
            FixedSize::Struct(val) => val.abi_type_name(),
        }
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        match self {
            FixedSize::Array(array) => array.abi_type_components(),
            FixedSize::Base(base) => base.abi_type_components(),
            FixedSize::Tuple(tuple) => tuple.abi_type_components(),
            FixedSize::String(string) => string.abi_type_components(),
            FixedSize::Contract(contract) => contract.abi_type_components(),
            FixedSize::Struct(val) => val.abi_type_components(),
        }
    }

    fn abi_safe_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_safe_name(),
            FixedSize::Base(base) => base.abi_safe_name(),
            FixedSize::Tuple(tuple) => tuple.abi_safe_name(),
            FixedSize::String(string) => string.abi_safe_name(),
            FixedSize::Contract(contract) => contract.abi_safe_name(),
            FixedSize::Struct(val) => val.abi_safe_name(),
        }
    }

    fn abi_type(&self) -> AbiType {
        match self {
            FixedSize::Base(base) => base.abi_type(),
            FixedSize::Array(array) => array.abi_type(),
            FixedSize::Tuple(tuple) => tuple.abi_type(),
            FixedSize::String(string) => string.abi_type(),
            FixedSize::Contract(contract) => contract.abi_type(),
            FixedSize::Struct(val) => val.abi_type(),
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

    pub fn bool() -> Self {
        FixedSize::Base(Base::Bool)
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
            Type::Struct(val) => Ok(FixedSize::Struct(val)),
            Type::Map(_) => Err(SemanticError::type_error()),
            Type::Contract(contract) => Ok(FixedSize::Contract(contract)),
        }
    }
}

impl FeSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::Numeric(integer) => integer.size(),
            Base::Bool => 1,
            Base::Byte => 1,
            Base::Address => 32,
        }
    }
}

impl AbiEncoding for Base {
    fn abi_type_name(&self) -> String {
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

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_safe_name(&self) -> String {
        self.abi_type_name()
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
                    data_size: 32,
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
        self.size * self.inner.size()
    }
}

impl AbiEncoding for Array {
    fn abi_type_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.size);
        }

        format!("{}[{}]", self.inner.abi_type_name(), self.size)
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_safe_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.size);
        }

        format!("{}{}", self.inner.abi_type_name(), self.size)
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(self.inner.abi_type()),
            size: AbiArraySize::Static { size: self.size },
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

impl FeSized for Struct {
    fn size(&self) -> usize {
        self.fields.len() * 32
    }
}

impl AbiEncoding for Struct {
    fn abi_type_name(&self) -> String {
        let field_names = self
            .get_field_types()
            .iter()
            .map(|typ| typ.abi_type_name())
            .collect::<Vec<String>>();
        let joined_names = field_names.join(",");
        format!("({})", joined_names)
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        self.order
            .iter()
            .map(|name| AbiComponent {
                name: name.to_owned(),
                typ: self.fields[name].abi_type_name(),
                components: vec![],
            })
            .collect()
    }

    fn abi_safe_name(&self) -> String {
        self.name.clone()
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Tuple {
            elems: self
                .get_field_types()
                .iter()
                .map(|typ| typ.abi_type())
                .collect(),
        }
    }
}

impl AbiEncoding for Tuple {
    fn abi_type_name(&self) -> String {
        unimplemented!();
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        unimplemented!()
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
    fn abi_type_name(&self) -> String {
        "string".to_string()
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        vec![]
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

impl FeSized for Contract {
    fn size(&self) -> usize {
        32
    }
}

impl AbiEncoding for Contract {
    fn abi_type_name(&self) -> String {
        unimplemented!();
    }

    fn abi_type_components(&self) -> Vec<AbiComponent> {
        unimplemented!()
    }

    fn abi_safe_name(&self) -> String {
        unimplemented!();
    }

    fn abi_type(&self) -> AbiType {
        unimplemented!();
    }
}

pub fn type_desc_fixed_size(
    defs: &HashMap<String, Type>,
    typ: &fe::TypeDesc,
) -> Result<FixedSize, SemanticError> {
    FixedSize::try_from(type_desc(defs, typ)?)
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
        fe::TypeDesc::Base { base } => match base.as_str() {
            "u256" => Ok(Type::Base(U256)),
            "u128" => Ok(Type::Base(Base::Numeric(Integer::U128))),
            "u64" => Ok(Type::Base(Base::Numeric(Integer::U64))),
            "u32" => Ok(Type::Base(Base::Numeric(Integer::U32))),
            "u16" => Ok(Type::Base(Base::Numeric(Integer::U16))),
            "u8" => Ok(Type::Base(Base::Numeric(Integer::U8))),
            "i256" => Ok(Type::Base(Base::Numeric(Integer::I256))),
            "i128" => Ok(Type::Base(Base::Numeric(Integer::I128))),
            "i64" => Ok(Type::Base(Base::Numeric(Integer::I64))),
            "i32" => Ok(Type::Base(Base::Numeric(Integer::I32))),
            "i16" => Ok(Type::Base(Base::Numeric(Integer::I16))),
            "i8" => Ok(Type::Base(Base::Numeric(Integer::I8))),
            "bool" => Ok(Type::Base(Base::Bool)),
            "bytes" => Ok(Type::Base(Base::Byte)),
            "address" => Ok(Type::Base(Base::Address)),
            base => {
                if base.starts_with("string") {
                    Ok(Type::String(
                        TryFrom::try_from(base).map_err(|_| SemanticError::type_error())?,
                    ))
                } else if let Some(typ) = defs.get(base) {
                    Ok(typ.clone())
                } else {
                    Err(SemanticError::undefined_value())
                }
            }
        },
        fe::TypeDesc::Array { typ, dimension } => Ok(Type::Array(Array {
            inner: type_desc_base(defs, &typ.kind)?,
            size: *dimension,
        })),
        fe::TypeDesc::Map { from, to } => Ok(Type::Map(Map {
            key: type_desc_base(defs, &from.kind)?,
            value: Box::new(type_desc(defs, &to.kind)?),
        })),
        fe::TypeDesc::Generic { .. } => todo!(),
        fe::TypeDesc::Tuple { items } => Ok(Type::Tuple(Tuple {
            items: items
                .iter()
                .map(|typ| type_desc_base(defs, &typ.kind))
                .collect::<Result<_, _>>()?,
        })),
    }
}
