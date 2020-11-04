use crate::errors::SemanticError;
use crate::namespace::scopes::*;
use fe_parser::ast as fe;
use std::collections::HashMap;

/// The type has a constant size known to the compiler.
pub trait FeSized {
    /// Constant size of the type.
    fn size(&self) -> usize;
}

/// The padding on an ABI type.
pub enum AbiPadding {
    /// The element is padded on the left with some number of bytes.
    Left { size: usize },
    /// The element is padded on the right with some number of bytes.
    Right { size: usize },
    /// There is no padding. For example, `u256` are already 32 bytes, so there
    /// is no need to pad them.
    None,
}

/// The type of an ABI element.
pub enum AbiType {
    /// Arrays are recursively encoded and consist of a single type.
    UniformRecursive { child: FixedSize, count: usize },
    /// Single values that do not require recursive encoding.
    Terminal,
}

/// Information relevant to ABI encoding.
pub trait AbiEncoding: FeSized {
    /// Name of the type as it appears in the Json ABI.
    fn abi_name(&self) -> String;

    /// Size of the type with ABI encoding padding.
    fn abi_size(&self) -> usize;

    /// Padding on the encoded data, if any.
    fn abi_padding(&self) -> AbiPadding;

    /// ABI type, either recursive or terminal.
    fn abi_type(&self) -> AbiType;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
    Tuple(Tuple),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
    Tuple(Tuple),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Base {
    U256,
    Bool,
    Byte,
    Address,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub dimension: usize,
    pub inner: Base,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub key: Base,
    pub value: Box<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    pub items: Vec<Base>,
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
        }
    }
}

impl AbiEncoding for FixedSize {
    fn abi_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_name(),
            FixedSize::Base(base) => base.abi_name(),
            FixedSize::Tuple(tuple) => tuple.abi_name(),
        }
    }

    fn abi_size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.abi_size(),
            FixedSize::Array(array) => array.abi_size(),
            FixedSize::Tuple(tuple) => tuple.abi_size(),
        }
    }

    fn abi_padding(&self) -> AbiPadding {
        match self {
            FixedSize::Base(base) => base.abi_padding(),
            FixedSize::Array(array) => array.abi_padding(),
            FixedSize::Tuple(tuple) => tuple.abi_padding(),
        }
    }

    fn abi_type(&self) -> AbiType {
        match self {
            FixedSize::Base(base) => base.abi_type(),
            FixedSize::Array(array) => array.abi_type(),
            FixedSize::Tuple(tuple) => tuple.abi_type(),
        }
    }
}

impl FixedSize {
    pub fn into_type(self) -> Type {
        match self {
            FixedSize::Array(array) => Type::Array(array),
            FixedSize::Base(base) => Type::Base(base),
            FixedSize::Tuple(tuple) => Type::Tuple(tuple),
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

    fn abi_size(&self) -> usize {
        match self {
            Base::U256 => 32,
            Base::Bool => 32,
            Base::Byte => 1,
            Base::Address => 32,
        }
    }

    fn abi_padding(&self) -> AbiPadding {
        match self {
            Base::U256 => AbiPadding::None,
            Base::Bool => AbiPadding::Left { size: 31 },
            Base::Byte => AbiPadding::None,
            Base::Address => AbiPadding::Left { size: 12 },
        }
    }

    fn abi_type(&self) -> AbiType {
        AbiType::Terminal
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

    fn abi_size(&self) -> usize {
        if self.inner == Base::Byte {
            if self.dimension % 32 == 0 {
                return self.dimension;
            }

            return (32 - (self.dimension % 32)) + self.dimension;
        }

        self.dimension * self.inner.abi_size()
    }

    fn abi_padding(&self) -> AbiPadding {
        if self.inner == Base::Byte {
            return AbiPadding::Right {
                size: self.abi_size() - self.size(),
            };
        }

        AbiPadding::None
    }

    fn abi_type(&self) -> AbiType {
        AbiType::UniformRecursive {
            child: FixedSize::Base(self.inner.clone()),
            count: self.dimension,
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

    fn abi_size(&self) -> usize {
        unimplemented!();
    }

    fn abi_padding(&self) -> AbiPadding {
        unimplemented!();
    }

    fn abi_type(&self) -> AbiType {
        unimplemented!();
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
