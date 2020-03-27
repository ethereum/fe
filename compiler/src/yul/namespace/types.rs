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
    pub fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size(),
        }
    }

    pub fn padding(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.padding(),
            FixedSize::Array(array) => array.padding(),
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

    pub fn padding(&self) -> usize {
        32 - self.size()
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

    pub fn padding(&self) -> usize {
        (self.dimension * self.inner.padding()) % 32
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

pub fn type_desc_fixed_size<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<FixedSize, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(FixedSize::Base(base)),
        Type::Array(array) => Ok(FixedSize::Array(array)),
        Type::Map(_) => Err(CompileError::static_str("Maps are not fixed size")),
    }
}

pub fn type_desc_base<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<Base, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        Type::Array(_) => Err(CompileError::static_str("Arrays are not a base type")),
        Type::Map(_) => Err(CompileError::static_str("Maps are not a base type")),
    }
}

pub fn type_desc_array<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<Array, CompileError> {
    match type_desc(defs, typ)? {
        Type::Array(array) => Ok(array),
        _ => Err(CompileError::static_str("Not an array")),
    }
}

pub fn type_desc_map<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<Map, CompileError> {
    match type_desc(defs, typ)? {
        Type::Map(map) => Ok(map),
        _ => Err(CompileError::static_str("Not a map")),
    }
}

pub fn type_desc<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<Type, CompileError> {
    match typ {
        vyp::TypeDesc::Base { base: "u256" } => Ok(Type::Base(Base::U256)),
        vyp::TypeDesc::Base { base: "bytes" } => Ok(Type::Base(Base::Byte)),
        vyp::TypeDesc::Base { base: "address" } => Ok(Type::Base(Base::Address)),
        vyp::TypeDesc::Base { base } => {
            if let Some(ModuleDef::Type(typ)) = defs.get(base.to_owned()) {
                return Ok(typ.clone());
            }

            Err(CompileError::static_str(
                "No type definition",
            ))
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
