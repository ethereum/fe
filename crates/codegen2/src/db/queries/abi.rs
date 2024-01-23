use fe_abi::{
    contract::AbiContract,
    event::{AbiEvent, AbiEventField},
    function::{AbiFunction, AbiFunctionType, CtxParam, SelfParam, StateMutability},
    types::{AbiTupleField, AbiType},
};
use fe_analyzer::{
    constants::INDEXED,
    namespace::{
        items::ContractId,
        types::{CtxDecl, SelfDecl},
    },
};
use fe_mir::ir::{self, FunctionId, TypeId};

use crate::db::CodegenDb;

pub fn abi_contract(db: &dyn CodegenDb, contract: ContractId) -> AbiContract {
    let mut funcs = vec![];

    if let Some(init) = contract.init_function(db.upcast()) {
        let init_func = db.mir_lowered_func_signature(init);
        let init_abi = db.codegen_abi_function(init_func);
        funcs.push(init_abi);
    }

    for &func in contract.all_functions(db.upcast()).as_ref() {
        let mir_func = db.mir_lowered_func_signature(func);
        if mir_func.linkage(db.upcast()).is_exported() {
            let func_abi = db.codegen_abi_function(mir_func);
            funcs.push(func_abi);
        }
    }

    let mut events = vec![];
    for &s in db.module_structs(contract.module(db.upcast())).as_ref() {
        let struct_ty = s.as_type(db.upcast());
        // TODO: This is a hack to avoid generating an ABI for non-`emittable` structs.
        if struct_ty.is_emittable(db.upcast()) {
            let mir_event = db.mir_lowered_type(struct_ty);
            let event = db.codegen_abi_event(mir_event);
            events.push(event);
        }
    }

    AbiContract::new(funcs, events)
}

pub fn abi_function(db: &dyn CodegenDb, function: FunctionId) -> AbiFunction {
    // We use a legalized signature.
    let sig = db.codegen_legalized_signature(function);

    let name = function.name(db.upcast());
    let args = sig
        .params
        .iter()
        .map(|param| (param.name.to_string(), db.codegen_abi_type(param.ty)))
        .collect();
    let ret_ty = sig.return_type.map(|ty| db.codegen_abi_type(ty));

    let func_type = if function.is_contract_init(db.upcast()) {
        AbiFunctionType::Constructor
    } else {
        AbiFunctionType::Function
    };

    // The "stateMutability" field is derived from the presence & mutability of
    // `self` and `ctx` params in the analyzer fn sig.
    let analyzer_sig = sig.analyzer_func_id.signature(db.upcast());
    let self_param = match analyzer_sig.self_decl {
        None => SelfParam::None,
        Some(SelfDecl { mut_: None, .. }) => SelfParam::Imm,
        Some(SelfDecl { mut_: Some(_), .. }) => SelfParam::Mut,
    };
    let ctx_param = match analyzer_sig.ctx_decl {
        None => CtxParam::None,
        Some(CtxDecl { mut_: None, .. }) => CtxParam::Imm,
        Some(CtxDecl { mut_: Some(_), .. }) => CtxParam::Mut,
    };

    let state_mutability = if name == "__init__" {
        StateMutability::Payable
    } else {
        StateMutability::from_self_and_ctx_params(self_param, ctx_param)
    };

    AbiFunction::new(func_type, name.to_string(), args, ret_ty, state_mutability)
}

pub fn abi_function_argument_maximum_size(db: &dyn CodegenDb, function: FunctionId) -> usize {
    let sig = db.codegen_legalized_signature(function);
    sig.params.iter().fold(0, |acc, param| {
        acc + db.codegen_abi_type_maximum_size(param.ty)
    })
}

pub fn abi_function_return_maximum_size(db: &dyn CodegenDb, function: FunctionId) -> usize {
    let sig = db.codegen_legalized_signature(function);
    sig.return_type
        .map(|ty| db.codegen_abi_type_maximum_size(ty))
        .unwrap_or_default()
}

pub fn abi_type_maximum_size(db: &dyn CodegenDb, ty: TypeId) -> usize {
    let abi_type = db.codegen_abi_type(ty);
    if abi_type.is_static() {
        abi_type.header_size()
    } else {
        match &ty.data(db.upcast()).kind {
            ir::TypeKind::Array(def) if def.elem_ty.data(db.upcast()).kind == ir::TypeKind::U8 => {
                debug_assert_eq!(abi_type, AbiType::Bytes);
                64 + ceil_32(def.len)
            }

            ir::TypeKind::Array(def) => {
                db.codegen_abi_type_maximum_size(def.elem_ty) * def.len + 32
            }

            ir::TypeKind::String(len) => abi_type.header_size() + 32 + ceil_32(*len),
            _ if ty.is_aggregate(db.upcast()) => {
                let mut maximum = 0;
                for i in 0..ty.aggregate_field_num(db.upcast()) {
                    let field_ty = ty.projection_ty_imm(db.upcast(), i);
                    maximum += db.codegen_abi_type_maximum_size(field_ty)
                }
                maximum + 32
            }
            ir::TypeKind::MPtr(ty) => abi_type_maximum_size(db, ty.deref(db.upcast())),

            _ => unreachable!(),
        }
    }
}

pub fn abi_type_minimum_size(db: &dyn CodegenDb, ty: TypeId) -> usize {
    let abi_type = db.codegen_abi_type(ty);
    if abi_type.is_static() {
        abi_type.header_size()
    } else {
        match &ty.data(db.upcast()).kind {
            ir::TypeKind::Array(def) if def.elem_ty.data(db.upcast()).kind == ir::TypeKind::U8 => {
                debug_assert_eq!(abi_type, AbiType::Bytes);
                64
            }
            ir::TypeKind::Array(def) => {
                db.codegen_abi_type_minimum_size(def.elem_ty) * def.len + 32
            }

            ir::TypeKind::String(_) => abi_type.header_size() + 32,

            _ if ty.is_aggregate(db.upcast()) => {
                let mut minimum = 0;
                for i in 0..ty.aggregate_field_num(db.upcast()) {
                    let field_ty = ty.projection_ty_imm(db.upcast(), i);
                    minimum += db.codegen_abi_type_minimum_size(field_ty)
                }
                minimum + 32
            }
            ir::TypeKind::MPtr(ty) => abi_type_minimum_size(db, ty.deref(db.upcast())),
            _ => unreachable!(),
        }
    }
}

pub fn abi_type(db: &dyn CodegenDb, ty: TypeId) -> AbiType {
    let legalized_ty = db.codegen_legalized_type(ty);

    if legalized_ty.is_zero_sized(db.upcast()) {
        unreachable!("zero-sized type must be removed in legalization");
    }

    let ty_data = legalized_ty.data(db.upcast());

    match &ty_data.kind {
        ir::TypeKind::I8 => AbiType::Int(8),
        ir::TypeKind::I16 => AbiType::Int(16),
        ir::TypeKind::I32 => AbiType::Int(32),
        ir::TypeKind::I64 => AbiType::Int(64),
        ir::TypeKind::I128 => AbiType::Int(128),
        ir::TypeKind::I256 => AbiType::Int(256),
        ir::TypeKind::U8 => AbiType::UInt(8),
        ir::TypeKind::U16 => AbiType::UInt(16),
        ir::TypeKind::U32 => AbiType::UInt(32),
        ir::TypeKind::U64 => AbiType::UInt(64),
        ir::TypeKind::U128 => AbiType::UInt(128),
        ir::TypeKind::U256 => AbiType::UInt(256),
        ir::TypeKind::Bool => AbiType::Bool,
        ir::TypeKind::Address => AbiType::Address,
        ir::TypeKind::String(_) => AbiType::String,
        ir::TypeKind::Unit => unreachable!("zero-sized type must be removed in legalization"),
        ir::TypeKind::Array(def) => {
            let elem_ty_data = &def.elem_ty.data(db.upcast());
            match &elem_ty_data.kind {
                ir::TypeKind::U8 => AbiType::Bytes,
                _ => {
                    let elem_ty = db.codegen_abi_type(def.elem_ty);
                    let len = def.len;
                    AbiType::Array {
                        elem_ty: elem_ty.into(),
                        len,
                    }
                }
            }
        }
        ir::TypeKind::Tuple(def) => {
            let fields = def
                .items
                .iter()
                .enumerate()
                .map(|(i, item)| {
                    let field_ty = db.codegen_abi_type(*item);
                    AbiTupleField::new(format!("{i}"), field_ty)
                })
                .collect();

            AbiType::Tuple(fields)
        }
        ir::TypeKind::Struct(def) => {
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
        ir::TypeKind::MPtr(inner) => db.codegen_abi_type(*inner),

        ir::TypeKind::Contract(_)
        | ir::TypeKind::Map(_)
        | ir::TypeKind::Enum(_)
        | ir::TypeKind::SPtr(_) => unreachable!(),
    }
}

pub fn abi_event(db: &dyn CodegenDb, ty: TypeId) -> AbiEvent {
    debug_assert!(ty.is_struct(db.upcast()));

    let legalized_ty = db.codegen_legalized_type(ty);
    let analyzer_struct = ty
        .analyzer_ty(db.upcast())
        .and_then(|val| val.as_struct(db.upcast()))
        .unwrap();
    let legalized_ty_data = legalized_ty.data(db.upcast());
    let event_def = match &legalized_ty_data.kind {
        ir::TypeKind::Struct(def) => def,
        _ => unreachable!(),
    };

    let fields = event_def
        .fields
        .iter()
        .map(|(name, ty)| {
            let attr = analyzer_struct
                .field(db.upcast(), name)
                .unwrap()
                .attributes(db.upcast());

            let ty = db.codegen_abi_type(*ty);
            let indexed = attr.iter().any(|attr| attr == INDEXED);
            AbiEventField::new(name.to_string(), ty, indexed)
        })
        .collect();

    AbiEvent::new(event_def.name.to_string(), fields, false)
}

fn ceil_32(value: usize) -> usize {
    ((value + 31) / 32) * 32
}
