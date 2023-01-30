mod abi;
mod contract;
mod data;
mod emit;
mod revert;
mod safe_math;

use std::fmt::Write;

use fe_abi::types::AbiType;
use fe_analyzer::namespace::items::ContractId;
use fe_mir::ir::{types::ArrayDef, FunctionId, TypeId, TypeKind};
use indexmap::IndexMap;
use yultsur::*;

use num_bigint::BigInt;

use crate::{db::CodegenDb, yul::slot_size::SLOT_SIZE};

use super::slot_size::yul_primitive_type;

pub trait RuntimeProvider {
    fn collect_definitions(&self) -> Vec<yul::FunctionDefinition>;

    fn alloc(&mut self, db: &dyn CodegenDb, size: yul::Expression) -> yul::Expression;

    fn avail(&mut self, db: &dyn CodegenDb) -> yul::Expression;

    fn create(
        &mut self,
        db: &dyn CodegenDb,
        contract: ContractId,
        value: yul::Expression,
    ) -> yul::Expression;

    fn create2(
        &mut self,
        db: &dyn CodegenDb,
        contract: ContractId,
        value: yul::Expression,
        salt: yul::Expression,
    ) -> yul::Expression;

    fn emit(
        &mut self,
        db: &dyn CodegenDb,
        event: yul::Expression,
        event_ty: TypeId,
    ) -> yul::Expression;

    fn revert(
        &mut self,
        db: &dyn CodegenDb,
        arg: Option<yul::Expression>,
        arg_name: &str,
        arg_ty: TypeId,
    ) -> yul::Expression;

    fn external_call(
        &mut self,
        db: &dyn CodegenDb,
        function: FunctionId,
        args: Vec<yul::Expression>,
    ) -> yul::Expression;

    fn map_value_ptr(
        &mut self,
        db: &dyn CodegenDb,
        map_ptr: yul::Expression,
        key: yul::Expression,
        key_ty: TypeId,
    ) -> yul::Expression;

    fn aggregate_init(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        args: Vec<yul::Expression>,
        ptr_ty: TypeId,
        arg_tys: Vec<TypeId>,
    ) -> yul::Expression;

    fn string_copy(
        &mut self,
        db: &dyn CodegenDb,
        dst: yul::Expression,
        data: &str,
        is_dst_storage: bool,
    ) -> yul::Expression;

    fn string_construct(
        &mut self,
        db: &dyn CodegenDb,
        data: &str,
        string_len: usize,
    ) -> yul::Expression;

    /// Copy data from `src` to `dst`.
    /// NOTE: src and dst must be aligned by 32 when a ptr is storage ptr.
    fn ptr_copy(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        dst: yul::Expression,
        size: yul::Expression,
        is_src_storage: bool,
        is_dst_storage: bool,
    ) -> yul::Expression;

    fn ptr_store(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        imm: yul::Expression,
        ptr_ty: TypeId,
    ) -> yul::Expression;

    fn ptr_load(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        ptr_ty: TypeId,
    ) -> yul::Expression;

    fn abi_encode(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        dst: yul::Expression,
        src_ty: TypeId,
        is_dst_storage: bool,
    ) -> yul::Expression;

    fn abi_encode_seq(
        &mut self,
        db: &dyn CodegenDb,
        src: &[yul::Expression],
        dst: yul::Expression,
        src_tys: &[TypeId],
        is_dst_storage: bool,
    ) -> yul::Expression;

    fn abi_decode(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        size: yul::Expression,
        types: &[TypeId],
        abi_loc: AbiSrcLocation,
    ) -> yul::Expression;

    fn primitive_cast(
        &mut self,
        db: &dyn CodegenDb,
        value: yul::Expression,
        from_ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(from_ty.is_primitive(db.upcast()));
        let from_size = from_ty.size_of(db.upcast(), SLOT_SIZE);

        if from_ty.is_signed(db.upcast()) {
            let significant = literal_expression! {(from_size-1)};
            expression! { signextend([significant], [value]) }
        } else {
            let mask = BitMask::new(from_size);
            expression! { and([value], [mask.as_expr()]) }
        }
    }

    // TODO: The all functions below will be reimplemented in `std`.
    fn safe_add(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;

    fn safe_sub(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;

    fn safe_mul(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;

    fn safe_div(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;

    fn safe_mod(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;

    fn safe_pow(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression;
}

#[derive(Clone, Copy, Debug)]
pub enum AbiSrcLocation {
    CallData,
    Memory,
}

#[derive(Debug, Default)]
pub struct DefaultRuntimeProvider {
    functions: IndexMap<String, RuntimeFunction>,
}

impl DefaultRuntimeProvider {
    fn create_then_call<F>(
        &mut self,
        name: &str,
        args: Vec<yul::Expression>,
        func_builder: F,
    ) -> yul::Expression
    where
        F: FnOnce(&mut Self) -> RuntimeFunction,
    {
        if let Some(func) = self.functions.get(name) {
            func.call(args)
        } else {
            let func = func_builder(self);
            let result = func.call(args);
            self.functions.insert(name.to_string(), func);
            result
        }
    }
}

impl RuntimeProvider for DefaultRuntimeProvider {
    fn collect_definitions(&self) -> Vec<yul::FunctionDefinition> {
        self.functions
            .values()
            .map(RuntimeFunction::definition)
            .collect()
    }

    fn alloc(&mut self, _db: &dyn CodegenDb, bytes: yul::Expression) -> yul::Expression {
        let name = "$alloc";
        let arg = vec![bytes];
        self.create_then_call(name, arg, |_| data::make_alloc(name))
    }

    fn avail(&mut self, _db: &dyn CodegenDb) -> yul::Expression {
        let name = "$avail";
        let arg = vec![];
        self.create_then_call(name, arg, |_| data::make_avail(name))
    }

    fn create(
        &mut self,
        db: &dyn CodegenDb,
        contract: ContractId,
        value: yul::Expression,
    ) -> yul::Expression {
        let name = format!("$create_{}", db.codegen_contract_symbol_name(contract));
        let arg = vec![value];
        self.create_then_call(&name, arg, |provider| {
            contract::make_create(provider, db, &name, contract)
        })
    }

    fn create2(
        &mut self,
        db: &dyn CodegenDb,
        contract: ContractId,
        value: yul::Expression,
        salt: yul::Expression,
    ) -> yul::Expression {
        let name = format!("$create2_{}", db.codegen_contract_symbol_name(contract));
        let arg = vec![value, salt];
        self.create_then_call(&name, arg, |provider| {
            contract::make_create2(provider, db, &name, contract)
        })
    }

    fn emit(
        &mut self,
        db: &dyn CodegenDb,
        event: yul::Expression,
        event_ty: TypeId,
    ) -> yul::Expression {
        let name = format!("$emit_{}", event_ty.0);
        let legalized_ty = db.codegen_legalized_type(event_ty);
        self.create_then_call(&name, vec![event], |provider| {
            emit::make_emit(provider, db, &name, legalized_ty)
        })
    }

    fn revert(
        &mut self,
        db: &dyn CodegenDb,
        arg: Option<yul::Expression>,
        arg_name: &str,
        arg_ty: TypeId,
    ) -> yul::Expression {
        let func_name = format! {"$revert_{}_{}", arg_name, arg_ty.0};
        let args = match arg {
            Some(arg) => vec![arg],
            None => vec![],
        };
        self.create_then_call(&func_name, args, |provider| {
            revert::make_revert(provider, db, &func_name, arg_name, arg_ty)
        })
    }

    fn external_call(
        &mut self,
        db: &dyn CodegenDb,
        function: FunctionId,
        args: Vec<yul::Expression>,
    ) -> yul::Expression {
        let name = format!(
            "$call_external__{}",
            db.codegen_function_symbol_name(function)
        );
        self.create_then_call(&name, args, |provider| {
            contract::make_external_call(provider, db, &name, function)
        })
    }

    fn map_value_ptr(
        &mut self,
        db: &dyn CodegenDb,
        map_ptr: yul::Expression,
        key: yul::Expression,
        key_ty: TypeId,
    ) -> yul::Expression {
        if key_ty.is_primitive(db.upcast()) {
            let name = "$map_value_ptr_with_primitive_key";
            self.create_then_call(name, vec![map_ptr, key], |provider| {
                data::make_map_value_ptr_with_primitive_key(provider, db, name, key_ty)
            })
        } else if key_ty.is_mptr(db.upcast()) {
            let name = "$map_value_ptr_with_ptr_key";
            self.create_then_call(name, vec![map_ptr, key], |provider| {
                data::make_map_value_ptr_with_ptr_key(provider, db, name, key_ty)
            })
        } else {
            unreachable!()
        }
    }

    fn aggregate_init(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        mut args: Vec<yul::Expression>,
        ptr_ty: TypeId,
        arg_tys: Vec<TypeId>,
    ) -> yul::Expression {
        debug_assert!(ptr_ty.is_ptr(db.upcast()));
        let deref_ty = ptr_ty.deref(db.upcast());

        // Handle unit enum variant.
        if args.len() == 1 && deref_ty.is_enum(db.upcast()) {
            let tag = args.pop().unwrap();
            let tag_ty = arg_tys[0];
            let is_sptr = ptr_ty.is_sptr(db.upcast());
            return self.ptr_store(db, ptr, tag, make_ptr(db, tag_ty, is_sptr));
        }

        let deref_ty = ptr_ty.deref(db.upcast());
        let args = std::iter::once(ptr).chain(args.into_iter()).collect();
        let legalized_ty = db.codegen_legalized_type(ptr_ty);
        if deref_ty.is_enum(db.upcast()) {
            let mut name = format!("enum_init_{}", ptr_ty.0);
            for ty in &arg_tys {
                write!(&mut name, "_{}", ty.0).unwrap();
            }
            self.create_then_call(&name, args, |provider| {
                data::make_enum_init(provider, db, &name, legalized_ty, arg_tys)
            })
        } else {
            let name = format!("$aggregate_init_{}", ptr_ty.0);
            self.create_then_call(&name, args, |provider| {
                data::make_aggregate_init(provider, db, &name, legalized_ty, arg_tys)
            })
        }
    }

    fn string_copy(
        &mut self,
        db: &dyn CodegenDb,
        dst: yul::Expression,
        data: &str,
        is_dst_storage: bool,
    ) -> yul::Expression {
        debug_assert!(data.is_ascii());
        let symbol_name = db.codegen_constant_string_symbol_name(data.to_string());

        let name = if is_dst_storage {
            format!("$string_copy_{symbol_name}_storage")
        } else {
            format!("$string_copy_{symbol_name}_memory")
        };

        self.create_then_call(&name, vec![dst], |provider| {
            data::make_string_copy(provider, db, &name, data, is_dst_storage)
        })
    }

    fn string_construct(
        &mut self,
        db: &dyn CodegenDb,
        data: &str,
        string_len: usize,
    ) -> yul::Expression {
        debug_assert!(data.is_ascii());
        debug_assert!(string_len >= data.len());
        let symbol_name = db.codegen_constant_string_symbol_name(data.to_string());

        let name = format!("$string_construct_{symbol_name}");
        let arg = literal_expression!((32 + string_len));
        self.create_then_call(&name, vec![arg], |provider| {
            data::make_string_construct(provider, db, &name, data)
        })
    }

    fn ptr_copy(
        &mut self,
        _db: &dyn CodegenDb,
        src: yul::Expression,
        dst: yul::Expression,
        size: yul::Expression,
        is_src_storage: bool,
        is_dst_storage: bool,
    ) -> yul::Expression {
        let args = vec![src, dst, size];
        match (is_src_storage, is_dst_storage) {
            (true, true) => {
                let name = "scopys";
                self.create_then_call(name, args, |_| data::make_scopys(name))
            }
            (true, false) => {
                let name = "scopym";
                self.create_then_call(name, args, |_| data::make_scopym(name))
            }
            (false, true) => {
                let name = "mcopys";
                self.create_then_call(name, args, |_| data::make_mcopys(name))
            }
            (false, false) => {
                let name = "mcopym";
                self.create_then_call(name, args, |_| data::make_mcopym(name))
            }
        }
    }

    fn ptr_store(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        imm: yul::Expression,
        ptr_ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ptr_ty.is_ptr(db.upcast()));
        let size = ptr_ty.deref(db.upcast()).size_of(db.upcast(), SLOT_SIZE);
        debug_assert!(size <= 32);

        let size_bits = size * 8;
        if ptr_ty.is_sptr(db.upcast()) {
            let name = "$sptr_store";
            let args = vec![ptr, imm, literal_expression! {(size_bits)}];
            self.create_then_call(name, args, |_| data::make_sptr_store(name))
        } else if ptr_ty.is_mptr(db.upcast()) {
            let name = "$mptr_store";
            let shift_num = literal_expression! {(256 - size_bits)};
            let mask = BitMask::new(32 - size);
            let args = vec![ptr, imm, shift_num, mask.as_expr()];
            self.create_then_call(name, args, |_| data::make_mptr_store(name))
        } else {
            unreachable!()
        }
    }

    fn ptr_load(
        &mut self,
        db: &dyn CodegenDb,
        ptr: yul::Expression,
        ptr_ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ptr_ty.is_ptr(db.upcast()));
        let size = ptr_ty.deref(db.upcast()).size_of(db.upcast(), SLOT_SIZE);
        debug_assert!(size <= 32);

        let size_bits = size * 8;
        if ptr_ty.is_sptr(db.upcast()) {
            let name = "$sptr_load";
            let args = vec![ptr, literal_expression! {(size_bits)}];
            self.create_then_call(name, args, |_| data::make_sptr_load(name))
        } else if ptr_ty.is_mptr(db.upcast()) {
            let name = "$mptr_load";
            let shift_num = literal_expression! {(256 - size_bits)};
            let args = vec![ptr, shift_num];
            self.create_then_call(name, args, |_| data::make_mptr_load(name))
        } else {
            unreachable!()
        }
    }

    fn abi_encode(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        dst: yul::Expression,
        src_ty: TypeId,
        is_dst_storage: bool,
    ) -> yul::Expression {
        let legalized_ty = db.codegen_legalized_type(src_ty);
        let args = vec![src.clone(), dst.clone()];

        let func_name_postfix = if is_dst_storage { "storage" } else { "memory" };

        if legalized_ty.is_primitive(db.upcast()) {
            let name = format!(
                "$abi_encode_primitive_type_{}_to_{}",
                src_ty.0, func_name_postfix
            );
            return self.create_then_call(&name, args, |provider| {
                abi::make_abi_encode_primitive_type(
                    provider,
                    db,
                    &name,
                    legalized_ty,
                    is_dst_storage,
                )
            });
        }

        let deref_ty = legalized_ty.deref(db.upcast());
        let abi_ty = db.codegen_abi_type(deref_ty);
        match abi_ty {
            AbiType::UInt(_) | AbiType::Int(_) | AbiType::Bool | AbiType::Address => {
                let value = self.ptr_load(db, src, src_ty);
                let extended_value = self.primitive_cast(db, value, deref_ty);
                self.abi_encode(db, extended_value, dst, deref_ty, is_dst_storage)
            }
            AbiType::Array { elem_ty, .. } => {
                if elem_ty.is_static() {
                    let name = format!(
                        "$abi_encode_static_array_type_{}_to_{}",
                        src_ty.0, func_name_postfix
                    );
                    self.create_then_call(&name, args, |provider| {
                        abi::make_abi_encode_static_array_type(provider, db, &name, legalized_ty)
                    })
                } else {
                    let name = format! {
                       "$abi_encode_dynamic_array_type{}_to_{}", src_ty.0, func_name_postfix
                    };
                    self.create_then_call(&name, args, |provider| {
                        abi::make_abi_encode_dynamic_array_type(provider, db, &name, legalized_ty)
                    })
                }
            }
            AbiType::Tuple(_) => {
                if abi_ty.is_static() {
                    let name = format!(
                        "$abi_encode_static_aggregate_type_{}_to_{}",
                        src_ty.0, func_name_postfix
                    );
                    self.create_then_call(&name, args, |provider| {
                        abi::make_abi_encode_static_aggregate_type(
                            provider,
                            db,
                            &name,
                            legalized_ty,
                            is_dst_storage,
                        )
                    })
                } else {
                    let name = format!(
                        "$abi_encode_dynamic_aggregate_type_{}_to_{}",
                        src_ty.0, func_name_postfix
                    );
                    self.create_then_call(&name, args, |provider| {
                        abi::make_abi_encode_dynamic_aggregate_type(
                            provider,
                            db,
                            &name,
                            legalized_ty,
                            is_dst_storage,
                        )
                    })
                }
            }
            AbiType::Bytes => {
                let len = match &deref_ty.data(db.upcast()).kind {
                    TypeKind::Array(ArrayDef { len, .. }) => *len,
                    _ => unreachable!(),
                };
                let name = format! {"$abi_encode_bytes{len}_type_to_{func_name_postfix}"};
                self.create_then_call(&name, args, |provider| {
                    abi::make_abi_encode_bytes_type(provider, db, &name, len, is_dst_storage)
                })
            }
            AbiType::String => {
                let name = format! {"$abi_encode_string_type_to_{func_name_postfix}"};
                self.create_then_call(&name, args, |provider| {
                    abi::make_abi_encode_string_type(provider, db, &name, is_dst_storage)
                })
            }
            AbiType::Function => unreachable!(),
        }
    }

    fn abi_encode_seq(
        &mut self,
        db: &dyn CodegenDb,
        src: &[yul::Expression],
        dst: yul::Expression,
        src_tys: &[TypeId],
        is_dst_storage: bool,
    ) -> yul::Expression {
        let mut name = "$abi_encode_value_seq".to_string();
        for ty in src_tys {
            write!(&mut name, "_{}", ty.0).unwrap();
        }

        let mut args = vec![dst];
        args.extend(src.iter().cloned());
        self.create_then_call(&name, args, |provider| {
            abi::make_abi_encode_seq(provider, db, &name, src_tys, is_dst_storage)
        })
    }

    fn abi_decode(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        size: yul::Expression,
        types: &[TypeId],
        abi_loc: AbiSrcLocation,
    ) -> yul::Expression {
        let mut name = "$abi_decode".to_string();
        for ty in types {
            write!(name, "_{}", ty.0).unwrap();
        }

        match abi_loc {
            AbiSrcLocation::CallData => write!(name, "_from_calldata").unwrap(),
            AbiSrcLocation::Memory => write!(name, "_from_memory").unwrap(),
        };

        self.create_then_call(&name, vec![src, size], |provider| {
            abi::make_abi_decode(provider, db, &name, types, abi_loc)
        })
    }

    fn safe_add(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_add(self, db, lhs, rhs, ty)
    }

    fn safe_sub(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_sub(self, db, lhs, rhs, ty)
    }

    fn safe_mul(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_mul(self, db, lhs, rhs, ty)
    }

    fn safe_div(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_div(self, db, lhs, rhs, ty)
    }

    fn safe_mod(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_mod(self, db, lhs, rhs, ty)
    }

    fn safe_pow(
        &mut self,
        db: &dyn CodegenDb,
        lhs: yul::Expression,
        rhs: yul::Expression,
        ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(ty.is_integral(db.upcast()));
        safe_math::dispatch_safe_pow(self, db, lhs, rhs, ty)
    }
}

#[derive(Debug)]
struct RuntimeFunction(yul::FunctionDefinition);

impl RuntimeFunction {
    fn arg_num(&self) -> usize {
        self.0.parameters.len()
    }

    fn definition(&self) -> yul::FunctionDefinition {
        self.0.clone()
    }

    /// # Panics
    /// Panics if a number of arguments doesn't match the definition.
    fn call(&self, args: Vec<yul::Expression>) -> yul::Expression {
        debug_assert_eq!(self.arg_num(), args.len());

        yul::Expression::FunctionCall(yul::FunctionCall {
            identifier: self.0.name.clone(),
            arguments: args,
        })
    }

    /// Remove this when `yultsur::function_definition!` becomes to return
    /// `FunctionDefinition`.
    fn from_statement(func: yul::Statement) -> Self {
        match func {
            yul::Statement::FunctionDefinition(def) => Self(def),
            _ => unreachable!(),
        }
    }
}

fn make_ptr(db: &dyn CodegenDb, inner: TypeId, is_sptr: bool) -> TypeId {
    if is_sptr {
        inner.make_sptr(db.upcast())
    } else {
        inner.make_mptr(db.upcast())
    }
}

struct BitMask(BigInt);

impl BitMask {
    fn new(byte_size: usize) -> Self {
        debug_assert!(byte_size <= 32);
        let one: BigInt = 1usize.into();
        Self((one << (byte_size * 8)) - 1)
    }

    fn not(&self) -> Self {
        // Bigint is variable length integer, so we need special handling for `not`
        // operation.
        let one: BigInt = 1usize.into();
        let u256_max = (one << 256) - 1;
        Self(u256_max ^ &self.0)
    }

    fn as_expr(&self) -> yul::Expression {
        let mask = format!("{:#x}", self.0);
        literal_expression! {(mask)}
    }
}

pub(super) fn error_revert_numeric(
    provider: &mut dyn RuntimeProvider,
    db: &dyn CodegenDb,
    error_code: yul::Expression,
) -> yul::Statement {
    yul::Statement::Expression(provider.revert(
        db,
        Some(error_code),
        "Error",
        yul_primitive_type(db),
    ))
}

pub(super) fn panic_revert_numeric(
    provider: &mut dyn RuntimeProvider,
    db: &dyn CodegenDb,
    error_code: yul::Expression,
) -> yul::Statement {
    yul::Statement::Expression(provider.revert(
        db,
        Some(error_code),
        "Panic",
        yul_primitive_type(db),
    ))
}
