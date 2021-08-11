use fe_analyzer::namespace::types::{
    Array, Base, Contract, FeString, FixedSize, Integer, Struct, Tuple,
};

pub trait EvmSized {
    /// The amount of bytes used by the type when being stored.
    fn size(&self) -> usize;
}

impl EvmSized for FixedSize {
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

impl EvmSized for Integer {
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

impl EvmSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::Numeric(integer) => integer.size(),
            Base::Bool => 1,
            Base::Address => 32,
            Base::Unit => 0,
        }
    }
}

impl EvmSized for Array {
    fn size(&self) -> usize {
        self.size * self.inner.size()
    }
}

impl EvmSized for Tuple {
    fn size(&self) -> usize {
        self.items.iter().map(|typ| typ.size()).sum()
    }
}

impl EvmSized for Struct {
    fn size(&self) -> usize {
        self.fields.len() * 32
    }
}

impl EvmSized for FeString {
    fn size(&self) -> usize {
        self.max_size + 32
    }
}

impl EvmSized for Contract {
    fn size(&self) -> usize {
        32
    }
}

/// Solidity ABI type with extra information needed for generation encoding/decoding functions.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum AbiType {
    StaticArray { inner: Box<AbiType>, size: usize },
    Tuple { components: Vec<AbiType> },
    Uint { size: usize },
    Int { size: usize },
    Bool,
    Address,
    String { max_size: usize },
    Bytes { size: usize },
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Copy)]
pub enum AbiDecodeLocation {
    Calldata,
    Memory,
}

pub fn to_abi_types<'a, T>(types: &'a [T]) -> Vec<AbiType>
where
    &'a T: Into<AbiType>,
{
    types.iter().map(|typ| typ.into()).collect()
}

pub fn to_abi_selector_names<'a, T>(types: &'a [T]) -> Vec<String>
where
    &'a T: Into<AbiType>,
{
    types.iter().map(|typ| typ.into().selector_name()).collect()
}

impl AbiType {
    /// The number of bytes used to encode the type's head.
    pub fn head_size(&self) -> usize {
        match self {
            AbiType::StaticArray { size, .. } => 32 * size,
            AbiType::Tuple { components } => 32 * components.len(),
            AbiType::Uint { .. } => 32,
            AbiType::Int { .. } => 32,
            AbiType::Bool => 32,
            AbiType::Address => 32,
            AbiType::String { .. } => 32,
            AbiType::Bytes { .. } => 32,
        }
    }

    /// The number of bytes used in Fe's data layout. This is used when packing and unpacking
    /// arrays.
    pub fn packed_size(&self) -> usize {
        match *self {
            AbiType::Uint { size } => size,
            AbiType::Int { size } => size,
            AbiType::Bool => 1,
            AbiType::Address => 32,
            _ => todo!("recursive encoding"),
        }
    }

    /// `true` if the encoded value is stored in the data section, `false` if it is not.
    pub fn has_data(&self) -> bool {
        match self {
            AbiType::Uint { .. } => false,
            AbiType::StaticArray { .. } => false,
            AbiType::Tuple { .. } => false,
            AbiType::Int { .. } => false,
            AbiType::Bool => false,
            AbiType::Address => false,
            AbiType::String { .. } => true,
            AbiType::Bytes { .. } => true,
        }
    }

    pub fn selector_name(&self) -> String {
        match self {
            AbiType::StaticArray { inner, size } => format!("{}[{}]", inner.selector_name(), size),
            AbiType::Tuple { components } => format!(
                "({})",
                components
                    .iter()
                    .map(|component| component.selector_name())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            AbiType::Uint { size } => format!("uint{}", 8 * size),
            AbiType::Int { size } => format!("int{}", 8 * size),
            AbiType::Bool => "bool".to_string(),
            AbiType::Address => "address".to_string(),
            AbiType::String { .. } => "string".to_string(),
            AbiType::Bytes { .. } => "bytes".to_string(),
        }
    }
}

impl From<&FixedSize> for AbiType {
    fn from(typ: &FixedSize) -> Self {
        match typ {
            FixedSize::Base(base) => base.into(),
            FixedSize::Array(array) => array.into(),
            FixedSize::Tuple(tuple) => tuple.into(),
            FixedSize::String(string) => string.into(),
            FixedSize::Contract(_) => AbiType::Address,
            FixedSize::Struct(val) => val.into(),
        }
    }
}

impl From<&Base> for AbiType {
    fn from(typ: &Base) -> Self {
        match typ {
            Base::Numeric(integer) => {
                let size = integer.size();
                if integer.is_signed() {
                    AbiType::Int { size }
                } else {
                    AbiType::Uint { size }
                }
            }
            Base::Address => AbiType::Address,
            Base::Bool => AbiType::Bool,
            Base::Unit => panic!("unit type is not abi encodable"),
        }
    }
}

impl From<&Array> for AbiType {
    fn from(array: &Array) -> Self {
        if matches!(array.inner, Base::Numeric(Integer::U8)) {
            AbiType::Bytes { size: array.size }
        } else {
            AbiType::StaticArray {
                inner: Box::new(AbiType::from(&array.inner)),
                size: array.size,
            }
        }
    }
}

impl From<&Struct> for AbiType {
    fn from(_struct: &Struct) -> Self {
        AbiType::Tuple {
            components: _struct.fields.iter().map(|(_, typ)| typ.into()).collect(),
        }
    }
}

impl From<&Tuple> for AbiType {
    fn from(tuple: &Tuple) -> Self {
        AbiType::Tuple {
            components: tuple.items.iter().map(|typ| typ.into()).collect(),
        }
    }
}

impl From<&FeString> for AbiType {
    fn from(string: &FeString) -> Self {
        AbiType::String {
            max_size: string.max_size,
        }
    }
}

impl From<&AbiType> for AbiType {
    fn from(typ: &AbiType) -> Self {
        typ.to_owned()
    }
}
