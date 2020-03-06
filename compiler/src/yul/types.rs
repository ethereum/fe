use vyper_parser::ast as vyp;
use yultsur::*;
use crate::errors::CompileError;
use std::collections::HashMap;
use crate::yul::ast_builder::ModuleDef;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Base(Base),
    Array(Array),
    Map(Map)
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixedSize {
    Base(Base),
    Array(Array),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Base {
    U256, U8, Address
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub dimension: usize,
    pub inner: Base
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    key: FixedSize,
    value: FixedSize,
}

impl FixedSize {
    pub fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size()
        }
    }
}

impl Base {
    pub fn size(&self) -> usize {
        match self {
            Base::U256 => 32,
            Base::U8 => 8,
            Base::Address => 20,
        }
    }
}

impl Array {
    pub fn size(&self) -> usize {
        self.dimension * self.inner.size()
    }
}

pub fn type_desc_fixed_size<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<FixedSize, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(FixedSize::Base(base)),
        Type::Array(array) => Ok(FixedSize::Array(array)),
        Type::Map(_) => Err(CompileError::static_str("Maps are not fixed size"))
    }
}

pub fn type_desc_base<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<Base, CompileError> {
    match type_desc(defs, typ)? {
        Type::Base(base) => Ok(base),
        Type::Array(_) => Err(CompileError::static_str("Arrays are not a base type")),
        Type::Map(_) => Err(CompileError::static_str("Maps are not a base type"))
    }
}

pub fn type_desc_array<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<Array, CompileError> {
    match type_desc(defs, typ)? {
        Type::Array(array) => Ok(array),
        _ => Err(CompileError::static_str("Not an array")),
    }
}

pub fn type_desc_map<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<Map, CompileError> {
    match type_desc(defs, typ)? {
        Type::Map(map) => Ok(map),
        _ => Err(CompileError::static_str("Not a map")),
    }
}

pub fn type_desc<'a>(
    defs: &HashMap<String, ModuleDef>,
    typ: &'a vyp::TypeDesc<'a>
) -> Result<Type, CompileError> {
    match typ {
        vyp::TypeDesc::Base { base: "u256" } => Ok(Type::Base(Base::U256)),
        vyp::TypeDesc::Base { base: "u8" } => Ok(Type::Base(Base::U8)),
        vyp::TypeDesc::Base { base: "address" } => Ok(Type::Base(Base::Address)),
        vyp::TypeDesc::Base { base } => {
            if let Some(ModuleDef::Type(typ)) = defs.get(base.to_owned()) {
                return Ok(typ.clone());
            }

            Err(CompileError::static_str("No type def for unknown base type"))
        },
        vyp::TypeDesc::Array { typ, dimension } => Ok(
            Type::Array(Array {
                inner: type_desc_base(defs, &typ.node)?,
                dimension: *dimension,
            })
        ),
        vyp::TypeDesc::Map { from, to } => Ok(
            Type::Map(Map {
                key: type_desc_fixed_size(defs, &to.node)?,
                value: type_desc_fixed_size(defs, &from.node)?,
            })
        ),
    }
}
