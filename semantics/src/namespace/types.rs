use crate::errors::SemanticError;
use crate::namespace::scopes::*;
use fe_parser::ast as fe;
use std::collections::HashMap;

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
    U256,
    Bool,
    Byte,
    Address,
}

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

impl Type {
    pub fn is_empty_tuple(&self) -> bool {
        if let Type::Tuple(tuple) = &self {
            return tuple.is_empty();
        }
        false
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
    pub fn into_type(self) -> Type {
        match self {
            FixedSize::Array(array) => Type::Array(array),
            FixedSize::Base(base) => Type::Base(base),
            FixedSize::Tuple(tuple) => Type::Tuple(tuple),
            FixedSize::String(string) => Type::String(string),
        }
    }

    pub fn is_empty_tuple(&self) -> bool {
        if let FixedSize::Tuple(tuple) = self {
            return tuple.is_empty();
        }

        false
    }
}

impl FeSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::U256 => 32,
            Base::Bool => 1,
            Base::Byte => 1,
            Base::Address => 20,
        }
    }
}

impl AbiEncoding for Base {
    fn abi_name(&self) -> String {
        match self {
            Base::U256 => "uint256".to_string(),
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
            Base::U256 => AbiType::Uint {
                size: AbiUintSize {
                    data_size: 32,
                    padded_size: 32,
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

impl Array {
    pub fn to_fixed_size(&self) -> FixedSize {
        FixedSize::Array(self.clone())
    }
}

impl Tuple {
    pub fn empty() -> Tuple {
        Tuple { items: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }

    pub fn to_fixed_size(&self) -> FixedSize {
        FixedSize::Tuple(self.clone())
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
    defs: &HashMap<String, ModuleDef>,
    typ: &fe::TypeDesc,
) -> Result<FixedSize, SemanticError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(FixedSize::Base(base)),
        Type::Array(array) => Ok(FixedSize::Array(array)),
        Type::Tuple(tuple) => Ok(FixedSize::Tuple(tuple)),
        Type::String(string) => Ok(FixedSize::String(string)),
        Type::Map(_) => Err(SemanticError::TypeError),
    }
}

pub fn type_desc_base(
    defs: &HashMap<String, ModuleDef>,
    typ: &fe::TypeDesc,
) -> Result<Base, SemanticError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        Type::Array(_) => Err(SemanticError::TypeError),
        Type::Map(_) => Err(SemanticError::TypeError),
        Type::Tuple(_) => Err(SemanticError::TypeError),
        Type::String(_) => Err(SemanticError::TypeError),
    }
}

pub fn type_desc(
    defs: &HashMap<String, ModuleDef>,
    typ: &fe::TypeDesc,
) -> Result<Type, SemanticError> {
    match typ {
        fe::TypeDesc::Base { base: "u256" } => Ok(Type::Base(Base::U256)),
        fe::TypeDesc::Base { base: "bool" } => Ok(Type::Base(Base::Bool)),
        fe::TypeDesc::Base { base: "bytes" } => Ok(Type::Base(Base::Byte)),
        fe::TypeDesc::Base { base: "address" } => Ok(Type::Base(Base::Address)),
        fe::TypeDesc::Base { base } if &base[..6] == "string" => {
            let max_size = base[6..]
                .parse::<u32>()
                .map_err(|_| SemanticError::TypeError)? as usize;
            Ok(Type::String(FeString { max_size }))
        }
        fe::TypeDesc::Base { base } => {
            if let Some(ModuleDef::Type(typ)) = defs.get(base.to_owned()) {
                return Ok(typ.clone());
            }

            Err(SemanticError::UndefinedValue {
                value: base.to_string(),
            })
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
