use std::rc::Rc;

use fe_mir::ir::{self, FunctionId, TypeId};
use fe_new_abi::{
    function::{AbiFunction, AbiFunctionType},
    types::{AbiTupleField, AbiType},
};

use crate::db::CodegenDb;

pub fn abi_function(db: &dyn CodegenDb, function: FunctionId) -> Rc<AbiFunction> {
    // We use a legalized signature.
    let sig = db.codegen_legalized_signature(function);

    let name = function.name(db.upcast());
    let args = sig
        .params
        .iter()
        .map(|param| (param.name.to_string(), db.codegen_abi_type(param.ty)))
        .collect();
    let ret_ty = sig.return_type.map(|ty| db.codegen_abi_type(ty));

    AbiFunction::new(AbiFunctionType::Function, name.to_string(), args, ret_ty).into()
}

pub fn abi_type(db: &dyn CodegenDb, ty: TypeId) -> AbiType {
    if ty.is_zero_sized(db.upcast()) {
        unreachable!("zero-sized type must be removed in legalization");
    }

    let ty = ty.data(db.upcast());

    match ty.as_ref() {
        ir::Type::I8 => AbiType::Int(8),
        ir::Type::I16 => AbiType::Int(16),
        ir::Type::I32 => AbiType::Int(32),
        ir::Type::I64 => AbiType::Int(64),
        ir::Type::I128 => AbiType::Int(128),
        ir::Type::I256 => AbiType::Int(256),
        ir::Type::U8 => AbiType::UInt(8),
        ir::Type::U16 => AbiType::UInt(16),
        ir::Type::U32 => AbiType::UInt(32),
        ir::Type::U64 => AbiType::UInt(64),
        ir::Type::U128 => AbiType::UInt(128),
        ir::Type::U256 => AbiType::UInt(256),
        ir::Type::Bool => AbiType::Bool,
        ir::Type::Address => AbiType::Address,
        ir::Type::Unit => unreachable!("zero-sized type must be removed in legalization"),
        ir::Type::Array(def) => {
            let elem_ty = db.codegen_abi_type(def.elem_ty);
            let len = def.len;
            AbiType::Array {
                elem_ty: elem_ty.into(),
                len,
            }
        }
        ir::Type::Tuple(def) => {
            let fields = def
                .items
                .iter()
                .enumerate()
                .map(|(i, item)| {
                    let field_ty = db.codegen_abi_type(*item);
                    AbiTupleField::new(format!("{}", i), field_ty)
                })
                .collect();

            AbiType::Tuple(fields)
        }
        ir::Type::Struct(def) => {
            let fields = def
                .fields
                .iter()
                .map(|(name, ty)| {
                    let ty = db.codegen_abi_type(*ty);
                    AbiTupleField::new(name.to_string(), ty)
                })
                .collect();

            AbiType::Tuple(fields)
        }
        ir::Type::Event(_) | ir::Type::Contract(_) => unreachable!(),
        ir::Type::Map(_) => todo!("map type can't be used in parameter or return type"),
    }
}
