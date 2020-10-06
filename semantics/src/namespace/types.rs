#![allow(dead_code)]
use crate::errors::SemanticError;
use crate::namespace::scopes::*;
use fe_parser::ast as fe;
use std::collections::HashMap;
use yultsur::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
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
    pub key: FixedSize,
    pub value: FixedSize,
}

impl FixedSize {
    pub fn padded_size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.padded_size(),
            FixedSize::Array(array) => array.padded_size(),
        }
    }

    pub fn decode(&self, ptr: yul::Expression) -> yul::Expression {
        match self {
            FixedSize::Base(base) => base.decode(ptr),
            FixedSize::Array(array) => array.decode(ptr),
        }
    }

    pub fn encode(&self, ptr: yul::Expression) -> yul::Expression {
        match self {
            FixedSize::Base(base) => base.encode(ptr),
            FixedSize::Array(array) => array.encode(ptr),
        }
    }

    pub fn abi_name(&self) -> String {
        match self {
            FixedSize::Array(array) => array.abi_name(),
            FixedSize::Base(base) => base.abi_name(),
        }
    }

    pub fn into_type(self) -> Type {
        match self {
            FixedSize::Array(array) => Type::Array(array),
            FixedSize::Base(base) => Type::Base(base),
        }
    }
}

impl Base {
    pub fn size(&self) -> usize {
        match self {
            Base::U256 => 32,
            Base::Bool => 1,
            Base::Byte => 1,
            Base::Address => 20,
        }
    }

    pub fn padded_size(&self) -> usize {
        32
    }

    pub fn abi_name(&self) -> String {
        match self {
            Base::U256 => "uint256".to_string(),
            Base::Address => "address".to_string(),
            Base::Byte => "byte".to_string(),
            Base::Bool => "bool".to_string(),
        }
    }

    pub fn decode(&self, ptr: yul::Expression) -> yul::Expression {
        expression! { calldataload([ptr]) }
    }

    pub fn encode(&self, val: yul::Expression) -> yul::Expression {
        expression! { alloc_mstoren([val], 32) }
    }

    pub fn mstore(&self, ptr: yul::Expression, value: yul::Expression) -> yul::Statement {
        let size = literal_expression! {(self.size())};

        statement! { mstoren([ptr], [value], [size]) }
    }

    pub fn mload(&self, ptr: yul::Expression) -> yul::Expression {
        let size = literal_expression! {(self.size())};

        expression! { mloadn([ptr], [size]) }
    }

    pub fn sstore(&self, ptr: yul::Expression, value: yul::Expression) -> yul::Statement {
        let size = literal_expression! {(self.size())};

        statement! { sstoren([ptr], [value], [size]) }
    }

    pub fn sload(&self, ptr: yul::Expression) -> yul::Expression {
        let size = literal_expression! {(self.size())};

        expression! { sloadn([ptr], [size]) }
    }
}

impl Array {
    pub fn size(&self) -> usize {
        self.dimension * self.inner.size()
    }

    pub fn padded_size(&self) -> usize {
        if self.inner == Base::Byte {
            if self.dimension % 32 == 0 {
                return self.dimension;
            }

            return (32 - (self.dimension % 32)) + self.dimension;
        }

        self.dimension * self.inner.padded_size()
    }

    pub fn decode(&self, ptr: yul::Expression) -> yul::Expression {
        let size = literal_expression! {(self.size())};

        match &self.inner {
            Base::Bool => expression! { ccopy([ptr], [size]) },
            Base::Byte => expression! { ccopy([ptr], [size]) },
            Base::U256 => expression! { ccopy([ptr], [size]) },
            Base::Address => unimplemented!("Address array decoding"),
        }
    }

    pub fn encode(&self, ptr: yul::Expression) -> yul::Expression {
        match &self.inner {
            Base::Bool => ptr,
            Base::Byte => ptr,
            Base::U256 => ptr,
            Base::Address => unimplemented!("Address array encoding"),
        }
    }

    pub fn abi_name(&self) -> String {
        if self.inner == Base::Byte {
            return format!("bytes{}", self.dimension);
        }

        format!("{}[{}]", self.inner.abi_name(), self.dimension)
    }

    pub fn mstore_elem(
        &self,
        array_ptr: yul::Expression,
        index: yul::Expression,
        value: yul::Expression,
    ) -> yul::Statement {
        let size = literal_expression! {(self.inner.size())};
        let value_ptr = expression! { add([array_ptr], (mul([index], [size]))) };

        self.inner.mstore(value_ptr, value)
    }

    pub fn mload_elem(
        &self,
        array_ptr: yul::Expression,
        index: yul::Expression,
    ) -> yul::Expression {
        let size = literal_expression! {(self.inner.size())};
        let value_ptr = expression! { add([array_ptr], (mul([index], [size]))) };

        self.inner.mload(value_ptr)
    }

    pub fn mcopy(&self, mptr: yul::Expression, sptr: yul::Expression) -> yul::Statement {
        let size = literal_expression! {(self.size())};

        statement! { mcopy([mptr], [sptr], [size]) }
    }

    pub fn scopy(&self, sptr: yul::Expression) -> yul::Expression {
        let size = literal_expression! {(self.size())};

        expression! { scopy([sptr], [size]) }
    }
}

impl Map {
    pub fn sstore(
        &self,
        index: usize,
        key: yul::Expression,
        value: yul::Expression,
    ) -> yul::Statement {
        let index = literal_expression! {(index)};
        let sptr = expression! { dualkeccak256([index], [key]) };

        match &self.value {
            FixedSize::Array(array) => array.mcopy(value, sptr),
            FixedSize::Base(base) => base.sstore(sptr, value),
        }
    }

    #[allow(dead_code)]
    pub fn sload(&self, index: usize, key: yul::Expression) -> yul::Expression {
        let index = literal_expression! {(index)};
        let sptr = expression! { dualkeccak256([index], [key]) };

        match &self.value {
            FixedSize::Array(array) => array.scopy(sptr),
            FixedSize::Base(base) => base.sload(sptr),
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

            Err(SemanticError::UndefinedValue)
        }
        fe::TypeDesc::Array { typ, dimension } => Ok(Type::Array(Array {
            inner: type_desc_base(defs, &typ.node)?,
            dimension: *dimension,
        })),
        fe::TypeDesc::Map { from, to } => Ok(Type::Map(Map {
            key: type_desc_fixed_size(defs, &from.node)?,
            value: type_desc_fixed_size(defs, &to.node)?,
        })),
    }
}
