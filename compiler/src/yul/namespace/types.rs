use crate::errors::CompileError;
use crate::yul::namespace::scopes::*;
use std::collections::HashMap;
use vyper_parser::ast as vyp;
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

    pub fn decode(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        match self {
            FixedSize::Base(base) => base.decode(ptr),
            FixedSize::Array(array) => array.decode(ptr),
        }
    }

    pub fn encode(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
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
}

impl Base {
    pub fn size(&self) -> usize {
        match self {
            Base::U256 => 32,
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
        }
    }

    pub fn decode(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        Ok(expression! { calldataload([ptr]) })
    }

    pub fn encode(&self, val: yul::Expression) -> Result<yul::Expression, CompileError> {
        Ok(expression! { alloc_mstoren([val], 32) })
    }

    pub fn mstore(
        &self,
        ptr: yul::Expression,
        value: yul::Expression,
    ) -> Result<yul::Statement, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(statement! { mstoren([ptr], [value], [size]) })
    }

    pub fn mload(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(expression! { mloadn([ptr], [size]) })
    }

    pub fn sstore(
        &self,
        ptr: yul::Expression,
        value: yul::Expression,
    ) -> Result<yul::Statement, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(statement! { sstoren([ptr], [value], [size]) })
    }

    pub fn sload(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(expression! { sloadn([ptr], [size]) })
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

    pub fn decode(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        let size = literal_expression! {(self.size())};

        match &self.inner {
            Base::Byte => Ok(expression! { ccopy([ptr], [size]) }),
            Base::U256 => Ok(expression! { ccopy([ptr], [size]) }),
            Base::Address => unimplemented!("Address array decoding"),
        }
    }

    pub fn encode(&self, ptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        match &self.inner {
            Base::Byte => Ok(ptr),
            Base::U256 => Ok(ptr),
            Base::Address => unimplemented!("Address array encoding")
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
        name: String,
        key: yul::Expression,
        value: yul::Expression,
    ) -> Result<yul::Statement, CompileError> {
        let name = literal_expression! {(name)};
        let size = literal_expression! {(self.inner.size())};
        let ptr = expression! { add([name], (mul([key],[size]))) };

        self.inner.mstore(ptr, value)
    }

    pub fn mload_elem(
        &self,
        name: String,
        key: yul::Expression,
    ) -> Result<yul::Expression, CompileError> {
        let name = literal_expression! {(name)};
        let size = literal_expression! {(self.inner.size())};
        let ptr = expression! { add([name], (mul([key],[size]))) };

        self.inner.mload(ptr)
    }

    pub fn mcopy(
        &self,
        mptr: yul::Expression,
        sptr: yul::Expression,
    ) -> Result<yul::Statement, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(statement! { mcopy([mptr], [sptr], [size]) })
    }

    pub fn scopy(&self, sptr: yul::Expression) -> Result<yul::Expression, CompileError> {
        let size = literal_expression! {(self.size())};

        Ok(expression! { scopy([sptr], [size]) })
    }
}

impl Map {
    pub fn sstore(
        &self,
        index: usize,
        key: yul::Expression,
        value: yul::Expression,
    ) -> Result<yul::Statement, CompileError> {
        let index = literal_expression! {(index)};
        let sptr = expression! { dualkeccak256([index], [key]) };

        match &self.value {
            FixedSize::Array(array) => array.mcopy(value, sptr),
            FixedSize::Base(base) => base.sstore(sptr, value),
        }
    }

    pub fn sload(
        &self,
        index: usize,
        key: yul::Expression,
    ) -> Result<yul::Expression, CompileError> {
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
    typ: &vyp::TypeDesc,
) -> Result<FixedSize, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(FixedSize::Base(base)),
        Type::Array(array) => Ok(FixedSize::Array(array)),
        Type::Map(_) => Err(CompileError::static_str("Maps are not fixed size")),
    }
}

pub fn type_desc_base(
    defs: &HashMap<String, ModuleDef>,
    typ: &vyp::TypeDesc,
) -> Result<Base, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        Type::Array(_) => Err(CompileError::static_str("Arrays are not a base type")),
        Type::Map(_) => Err(CompileError::static_str("Maps are not a base type")),
    }
}

pub fn type_desc(
    defs: &HashMap<String, ModuleDef>,
    typ: &vyp::TypeDesc,
) -> Result<Type, CompileError> {
    match typ {
        vyp::TypeDesc::Base { base: "u256" } => Ok(Type::Base(Base::U256)),
        vyp::TypeDesc::Base { base: "bytes" } => Ok(Type::Base(Base::Byte)),
        vyp::TypeDesc::Base { base: "address" } => Ok(Type::Base(Base::Address)),
        vyp::TypeDesc::Base { base } => {
            if let Some(ModuleDef::Type(typ)) = defs.get(base.to_owned()) {
                return Ok(typ.clone());
            }

            Err(CompileError::static_str("No type definition"))
        }
        vyp::TypeDesc::Array { typ, dimension } => Ok(Type::Array(Array {
            inner: type_desc_base(defs, &typ.node)?,
            dimension: *dimension,
        })),
        vyp::TypeDesc::Map { from, to } => Ok(Type::Map(Map {
            key: type_desc_fixed_size(defs, &from.node)?,
            value: type_desc_fixed_size(defs, &to.node)?,
        })),
    }
}
