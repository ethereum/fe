use crate::errors::{ErrorKind, SemanticError};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::collections::{btree_map::Entry, BTreeMap};
use std::convert::TryFrom;

use crate::context::FunctionAttributes;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use strum::IntoStaticStr;
use vec1::Vec1;

pub fn u256_min() -> BigInt {
    BigInt::from(0)
}

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
pub trait AbiEncoding: SafeNames {
    /// Name of the type as it appears in the Json ABI.
    fn abi_json_name(&self) -> String;

    /// Name of the type as it appears in the selector preimage.
    fn abi_selector_name(&self) -> String;

    /// The components of an ABI tuple.
    fn abi_components(&self) -> Vec<AbiComponent>;

    /// The ABI type of a Fe type.
    fn abi_type(&self) -> AbiType;
}

/// Names that can be used to build identifiers without collision.
pub trait SafeNames {
    /// Name in the lower snake format (e.g. lower_snake_case).
    fn lower_snake(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
    Tuple(Tuple),
    String(FeString),
    Contract(Contract),
    Struct(Struct),
    Unit,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
    Tuple(Tuple),
    String(FeString),
    Contract(Contract),
    Struct(Struct),
    Unit,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Array {
    pub size: usize,
    pub inner: Base,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Map {
    pub key: Base,
    pub value: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Tuple {
    pub items: Vec1<FixedSize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Struct {
    pub name: String,
    fields: BTreeMap<String, FixedSize>,
    order: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct FeString {
    pub max_size: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
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

    /// Return `true` if the struct does not have any fields, otherwise return `false`
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
    pub fn fits(&self, num: BigInt) -> bool {
        match self {
            Integer::U8 => num.to_u8().is_some(),
            Integer::U16 => num.to_u16().is_some(),
            Integer::U32 => num.to_u32().is_some(),
            Integer::U64 => num.to_u64().is_some(),
            Integer::U128 => num.to_u128().is_some(),
            Integer::I8 => num.to_i8().is_some(),
            Integer::I16 => num.to_i16().is_some(),
            Integer::I32 => num.to_i32().is_some(),
            Integer::I64 => num.to_i64().is_some(),
            Integer::I128 => num.to_i128().is_some(),
            Integer::U256 => num >= u256_min() && num <= u256_max(),
            Integer::I256 => num >= i256_min() && num <= i256_max(),
        }
    }
}

impl Type {
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
            FixedSize::Unit => Type::Unit,
        }
    }
}

impl From<Base> for Type {
    fn from(value: Base) -> Self {
        Type::Base(value)
    }
}

impl From<Base> for FixedSize {
    fn from(value: Base) -> Self {
        FixedSize::Base(value)
    }
}

impl FixedSize {
    /// Returns true if the type is `()`.
    pub fn is_unit(&self) -> bool {
        self == &Self::Unit
    }

    /// Creates an instance of bool.
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
            Type::Unit => Ok(FixedSize::Unit),
        }
    }
}

impl From<Tuple> for FixedSize {
    fn from(value: Tuple) -> Self {
        FixedSize::Tuple(value)
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
            FixedSize::Unit => 0,
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

impl FeSized for Array {
    fn size(&self) -> usize {
        self.size * self.inner.size()
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

impl FeSized for FeString {
    fn size(&self) -> usize {
        self.max_size + 32
    }
}

impl FeSized for Contract {
    fn size(&self) -> usize {
        32
    }
}

impl AbiEncoding for FixedSize {
    fn abi_json_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_json_name(),
            FixedSize::Base(base) => base.abi_json_name(),
            FixedSize::Tuple(tuple) => tuple.abi_json_name(),
            FixedSize::String(string) => string.abi_json_name(),
            FixedSize::Contract(contract) => contract.abi_json_name(),
            FixedSize::Struct(val) => val.abi_json_name(),
            FixedSize::Unit => panic!("unit is not encodable"),
        }
    }

    fn abi_selector_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_selector_name(),
            FixedSize::Base(base) => base.abi_selector_name(),
            FixedSize::Tuple(tuple) => tuple.abi_selector_name(),
            FixedSize::String(string) => string.abi_selector_name(),
            FixedSize::Contract(contract) => contract.abi_selector_name(),
            FixedSize::Struct(val) => val.abi_selector_name(),
            FixedSize::Unit => panic!("unit is not encodable"),
        }
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        match self {
            FixedSize::Array(array) => array.abi_components(),
            FixedSize::Base(base) => base.abi_components(),
            FixedSize::Tuple(tuple) => tuple.abi_components(),
            FixedSize::String(string) => string.abi_components(),
            FixedSize::Contract(contract) => contract.abi_components(),
            FixedSize::Struct(val) => val.abi_components(),
            FixedSize::Unit => panic!("unit is not encodable"),
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
            FixedSize::Unit => panic!("unit is not encodable"),
        }
    }
}

impl AbiEncoding for Base {
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
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
        }
    }

    fn abi_selector_name(&self) -> String {
        self.abi_json_name()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
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

impl AbiEncoding for Array {
    fn abi_json_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.size);
        }

        format!("{}[{}]", self.inner.abi_json_name(), self.size)
    }

    fn abi_selector_name(&self) -> String {
        self.abi_json_name()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Array {
            inner: Box::new(self.inner.abi_type()),
            size: AbiArraySize::Static { size: self.size },
        }
    }
}

impl AbiEncoding for Struct {
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }

    fn abi_selector_name(&self) -> String {
        let field_names = self
            .get_field_types()
            .iter()
            .map(|typ| typ.abi_json_name())
            .collect::<Vec<String>>();
        let joined_names = field_names.join(",");
        format!("({})", joined_names)
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        self.order
            .iter()
            .map(|name| AbiComponent {
                name: name.to_owned(),
                typ: self.fields[name].abi_json_name(),
                components: vec![],
            })
            .collect()
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
    fn abi_json_name(&self) -> String {
        "tuple".to_string()
    }

    fn abi_selector_name(&self) -> String {
        let field_names = self
            .items
            .iter()
            .map(|typ| typ.abi_json_name())
            .collect::<Vec<String>>();
        let joined_names = field_names.join(",");
        format!("({})", joined_names)
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        self.items
            .iter()
            .enumerate()
            .map(|(index, item)| AbiComponent {
                name: format!("item{}", index),
                typ: item.abi_json_name(),
                components: vec![],
            })
            .collect()
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Tuple {
            elems: self.items.iter().map(|typ| typ.abi_type()).collect(),
        }
    }
}

impl AbiEncoding for Contract {
    fn abi_json_name(&self) -> String {
        unimplemented!();
    }

    fn abi_selector_name(&self) -> String {
        unimplemented!()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        unimplemented!()
    }

    fn abi_type(&self) -> AbiType {
        unimplemented!();
    }
}

impl AbiEncoding for FeString {
    fn abi_json_name(&self) -> String {
        "string".to_string()
    }

    fn abi_selector_name(&self) -> String {
        "string".to_string()
    }

    fn abi_components(&self) -> Vec<AbiComponent> {
        vec![]
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

impl SafeNames for FixedSize {
    fn lower_snake(&self) -> String {
        match self {
            FixedSize::Array(array) => array.lower_snake(),
            FixedSize::Base(base) => base.lower_snake(),
            FixedSize::Tuple(tuple) => tuple.lower_snake(),
            FixedSize::String(string) => string.lower_snake(),
            FixedSize::Contract(contract) => contract.lower_snake(),
            FixedSize::Struct(val) => val.lower_snake(),
            FixedSize::Unit => "unit".to_string(),
        }
    }
}

impl SafeNames for Base {
    fn lower_snake(&self) -> String {
        match self {
            Base::Numeric(Integer::U256) => "u256".to_string(),
            Base::Numeric(Integer::U128) => "u128".to_string(),
            Base::Numeric(Integer::U64) => "u64".to_string(),
            Base::Numeric(Integer::U32) => "u32".to_string(),
            Base::Numeric(Integer::U16) => "u16".to_string(),
            Base::Numeric(Integer::U8) => "u8".to_string(),
            Base::Numeric(Integer::I256) => "i256".to_string(),
            Base::Numeric(Integer::I128) => "i128".to_string(),
            Base::Numeric(Integer::I64) => "i64".to_string(),
            Base::Numeric(Integer::I32) => "i32".to_string(),
            Base::Numeric(Integer::I16) => "i16".to_string(),
            Base::Numeric(Integer::I8) => "i8".to_string(),
            Base::Address => "address".to_string(),
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
        }
    }
}

impl SafeNames for Array {
    fn lower_snake(&self) -> String {
        format!("array_{}_{}", self.inner.lower_snake(), self.size)
    }
}

impl SafeNames for Struct {
    fn lower_snake(&self) -> String {
        format!("struct_{}", self.name)
    }
}

impl SafeNames for Tuple {
    fn lower_snake(&self) -> String {
        let field_names = self
            .items
            .iter()
            .map(|typ| typ.lower_snake())
            .collect::<Vec<String>>();
        let joined_names = field_names.join("_");

        format!("tuple_{}", joined_names)
    }
}

impl SafeNames for Contract {
    fn lower_snake(&self) -> String {
        unimplemented!();
    }
}

impl SafeNames for FeString {
    fn lower_snake(&self) -> String {
        format!("string_{}", self.max_size)
    }
}

pub fn type_desc_fixed_size(
    defs: &BTreeMap<String, Type>,
    typ: &fe::TypeDesc,
) -> Result<FixedSize, SemanticError> {
    FixedSize::try_from(type_desc(defs, typ)?)
}

pub fn type_desc_base(
    defs: &BTreeMap<String, Type>,
    typ: &fe::TypeDesc,
) -> Result<Base, SemanticError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        _ => Err(SemanticError::type_error()),
    }
}

pub fn type_desc(defs: &BTreeMap<String, Type>, typ: &fe::TypeDesc) -> Result<Type, SemanticError> {
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
                if let Some(typ) = defs.get(base) {
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
        fe::TypeDesc::Generic { base, args } => {
            if base.kind == "map" {
                match &args[..] {
                    [Node {
                        kind: fe::GenericArg::TypeDesc(from),
                        ..
                    }, Node {
                        kind: fe::GenericArg::TypeDesc(to),
                        ..
                    }] => Ok(Type::Map(Map {
                        key: type_desc_base(defs, &from)?,
                        value: Box::new(type_desc(defs, &to)?),
                    })),
                    _ => Err(SemanticError {
                        kind: ErrorKind::MapTypeError,
                        context: vec![],
                    }),
                }
            } else if base.kind == "String" {
                match &args[..] {
                    [Node {
                        kind: fe::GenericArg::Int(len),
                        ..
                    }] => Ok(Type::String(FeString { max_size: *len })),
                    _ => Err(SemanticError::type_error()),
                }
            } else {
                Err(SemanticError::undefined_value())
            }
        }
        fe::TypeDesc::Tuple { items } => Ok(Type::Tuple(Tuple {
            items: Vec1::try_from_vec(
                items
                    .iter()
                    .map(|typ| type_desc_fixed_size(defs, &typ.kind))
                    .collect::<Result<_, _>>()?,
            )
            .expect("tuple is empty"),
        })),
        fe::TypeDesc::Unit => Ok(Type::Unit),
    }
}
