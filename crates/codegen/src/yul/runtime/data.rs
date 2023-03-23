use crate::{
    db::CodegenDb,
    yul::{
        runtime::{make_ptr, BitMask},
        slot_size::{yul_primitive_type, SLOT_SIZE},
        YulVariable,
    },
};

use super::{DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_mir::ir::{types::TupleDef, Type, TypeId, TypeKind};

use yultsur::*;

const HASH_SCRATCH_SPACE_START: usize = 0x00;
const HASH_SCRATCH_SPACE_SIZE: usize = 64;
const FREE_MEMORY_ADDRESS_STORE: usize = HASH_SCRATCH_SPACE_START + HASH_SCRATCH_SPACE_SIZE;
const FREE_MEMORY_START: usize = FREE_MEMORY_ADDRESS_STORE + 32;

pub(super) fn make_alloc(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let free_address_ptr = literal_expression! {(FREE_MEMORY_ADDRESS_STORE)};
    let free_memory_start = literal_expression! {(FREE_MEMORY_START)};
    let func = function_definition! {
        function [func_name.ident()](size) -> ptr {
            (ptr := mload([free_address_ptr.clone()]))
            (if (eq(ptr, 0x00)) { (ptr := [free_memory_start]) })
            (mstore([free_address_ptr], (add(ptr, size))))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_avail(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let free_address_ptr = literal_expression! {(FREE_MEMORY_ADDRESS_STORE)};
    let free_memory_start = literal_expression! {(FREE_MEMORY_START)};
    let func = function_definition! {
        function [func_name.ident()]() -> ptr {
            (ptr := mload([free_address_ptr]))
            (if (eq(ptr, 0x00)) { (ptr := [free_memory_start]) })
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_mcopym(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let size = YulVariable::new("size");

    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()], [size.ident()]) {
            (let iter_count := div([size.expr()], 32))
            (let original_src := [src.expr()])
            (for {(let i := 0)} (lt(i, iter_count)) {(i := (add(i, 1)))}
            {
                (mstore([dst.expr()], (mload([src.expr()]))))
                ([src.ident()] := add([src.expr()], 32))
                ([dst.ident()] := add([dst.expr()], 32))
            })

            (let rem := sub([size.expr()], (sub([src.expr()], original_src))))
            (if (gt(rem, 0)) {
                (let rem_bits := mul(rem, 8))
                (let dst_mask := sub((shl((sub(256, rem_bits)), 1)), 1))
                (let src_mask := not(dst_mask))
                (let src_value := and((mload([src.expr()])), src_mask))
                (let dst_value := and((mload([dst.expr()])), dst_mask))
                (mstore([dst.expr()], (or(src_value, dst_value))))
            })
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_mcopys(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let size = YulVariable::new("size");

    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()], [size.ident()]) {
            ([dst.ident()] := div([dst.expr()], 32))
            (let iter_count := div([size.expr()], 32))
            (let original_src := [src.expr()])
            (for {(let i := 0)} (lt(i, iter_count)) {(i := (add(i, 1)))}
            {
                (sstore([dst.expr()], (mload([src.expr()]))))
                ([src.ident()] := add([src.expr()], 32))
                ([dst.ident()] := add([dst.expr()], 1))
            })

            (let rem := sub([size.expr()], (sub([src.expr()], original_src))))
            (if (gt(rem, 0)) {
                (let rem_bits := mul(rem, 8))
                (let dst_mask := sub((shl((sub(256, rem_bits)), 1)), 1))
                (let src_mask := not(dst_mask))
                (let src_value := and((mload([src.expr()])), src_mask))
                (let dst_value := and((sload([dst.expr()])), dst_mask))
                (sstore([dst.expr()], (or(src_value, dst_value))))
            })
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_scopym(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let size = YulVariable::new("size");

    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()], [size.ident()]) {
            ([src.ident()] := div([src.expr()], 32))
            (let iter_count := div([size.expr()], 32))
            (let original_dst := [dst.expr()])
            (for {(let i := 0)} (lt(i, iter_count)) {(i := (add(i, 1)))}
            {
                (mstore([dst.expr()], (sload([src.expr()]))))
                ([src.ident()] := add([src.expr()], 1))
                ([dst.ident()] := add([dst.expr()], 32))
            })

            (let rem := sub([size.expr()], (sub([dst.expr()], original_dst))))
            (if (gt(rem, 0)) {
                (let rem_bits := mul(rem, 8))
                (let dst_mask := sub((shl((sub(256, rem_bits)), 1)), 1))
                (let src_mask := not(dst_mask))
                (let src_value := and((sload([src.expr()])), src_mask))
                (let dst_value := and((mload([dst.expr()])), dst_mask))
                (mstore([dst.expr()], (or(src_value, dst_value))))
            })
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_scopys(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let size = YulVariable::new("size");
    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()], [size.ident()]) {
            ([src.ident()] := div([src.expr()], 32))
            ([dst.ident()] := div([dst.expr()], 32))
            (let iter_count := div((add([size.expr()], 31)), 32))
            (for {(let i := 0)} (lt(i, iter_count)) {(i := (add(i, 1)))}
            {
                (sstore([dst.expr()], (sload([src.expr()]))))
                ([src.ident()] := add([src.expr()], 1))
                ([dst.ident()] := add([dst.expr()], 1))
            })
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_sptr_store(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let func = function_definition! {
        function [func_name.ident()](ptr, value, size_bits) {
            (let rem_bits := mul((mod(ptr, 32)), 8))
            (let shift_bits := sub(256, (add(rem_bits, size_bits))))
            (let mask := (shl(shift_bits, (sub((shl(size_bits, 1)), 1)))))
            (let inv_mask := not(mask))
            (let slot := div(ptr, 32))
            (let new_value := or((and((sload(slot)), inv_mask)), (and((shl(shift_bits, value)), mask))))
            (sstore(slot, new_value))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_mptr_store(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let func = function_definition! {
        function [func_name.ident()](ptr, value, shift_num, mask) {
            (value := shl(shift_num, value))
            (let ptr_value := and((mload(ptr)), mask))
            (value := or(value, ptr_value))
            (mstore(ptr, value))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_sptr_load(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let func = function_definition! {
        function [func_name.ident()](ptr, size_bits) -> ret {
            (let rem_bits := mul((mod(ptr, 32)), 8))
            (let shift_num := sub(256, (add(rem_bits, size_bits))))
            (let slot := div(ptr, 32))
            (ret := shr(shift_num, (sload(slot))))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_mptr_load(func_name: &str) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let func = function_definition! {
        function [func_name.ident()](ptr, shift_num) -> ret {
            (ret := shr(shift_num, (mload(ptr))))
        }
    };

    RuntimeFunction::from_statement(func)
}

// TODO: We can optimize aggregate initialization by combining multiple
// `ptr_store` operations into single `ptr_store` operation.
pub(super) fn make_aggregate_init(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    arg_tys: Vec<TypeId>,
) -> RuntimeFunction {
    debug_assert!(legalized_ty.is_ptr(db.upcast()));
    let is_sptr = legalized_ty.is_sptr(db.upcast());
    let inner_ty = legalized_ty.deref(db.upcast());
    let ptr = YulVariable::new("ptr");
    let field_num = inner_ty.aggregate_field_num(db.upcast());

    let iter_field_args = || (0..field_num).map(|i| YulVariable::new(format! {"arg{i}"}));

    let mut body = vec![];
    for (idx, field_arg) in iter_field_args().enumerate() {
        let field_arg_ty = arg_tys[idx];
        let field_ty = inner_ty
            .projection_ty_imm(db.upcast(), idx)
            .deref(db.upcast());
        let field_ty_size = field_ty.size_of(db.upcast(), SLOT_SIZE);
        let field_ptr_ty = make_ptr(db, field_ty, is_sptr);
        let field_offset =
            literal_expression! {(inner_ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE))};

        let field_ptr = expression! { add([ptr.expr()], [field_offset] )};
        let copy_expr = if field_ty.is_aggregate(db.upcast()) || field_ty.is_string(db.upcast()) {
            // Call ptr copy function if field type is aggregate.
            debug_assert!(field_arg_ty.is_ptr(db.upcast()));
            provider.ptr_copy(
                db,
                field_arg.expr(),
                field_ptr,
                literal_expression! {(field_ty_size)},
                field_arg_ty.is_sptr(db.upcast()),
                is_sptr,
            )
        } else {
            // Call store function if field type is not aggregate.
            provider.ptr_store(db, field_ptr, field_arg.expr(), field_ptr_ty)
        };
        body.push(yul::Statement::Expression(copy_expr));
    }

    let func_name = identifier! {(func_name)};
    let parameters = std::iter::once(ptr)
        .chain(iter_field_args())
        .map(|var| var.ident())
        .collect();
    let func_def = yul::FunctionDefinition {
        name: func_name,
        parameters,
        returns: vec![],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

pub(super) fn make_enum_init(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    arg_tys: Vec<TypeId>,
) -> RuntimeFunction {
    debug_assert!(arg_tys.len() > 1);

    let func_name = YulVariable::new(func_name);
    let is_sptr = legalized_ty.is_sptr(db.upcast());
    let ptr = YulVariable::new("ptr");
    let disc = YulVariable::new("disc");
    let disc_ty = arg_tys[0];
    let enum_data = || (0..arg_tys.len() - 1).map(|i| YulVariable::new(format! {"arg{i}"}));

    let tuple_def = TupleDef {
        items: arg_tys
            .iter()
            .map(|ty| ty.deref(db.upcast()))
            .skip(1)
            .collect(),
    };
    let tuple_ty = db.mir_intern_type(
        Type {
            kind: TypeKind::Tuple(tuple_def),
            analyzer_ty: None,
        }
        .into(),
    );
    let data_ptr_ty = make_ptr(db, tuple_ty, is_sptr);
    let data_offset = legalized_ty
        .deref(db.upcast())
        .enum_data_offset(db.upcast(), SLOT_SIZE);
    let enum_data_init = statements! {
        [statement! {[ptr.ident()] := add([ptr.expr()], [literal_expression!{(data_offset)}])}]
        [yul::Statement::Expression(provider.aggregate_init(
        db,
        ptr.expr(),
        enum_data().map(|arg| arg.expr()).collect(),
        data_ptr_ty, arg_tys.iter().skip(1).copied().collect()))]
    };

    let enum_data_args: Vec<_> = enum_data().map(|var| var.ident()).collect();
    let func_def = function_definition! {
        function [func_name.ident()]([ptr.ident()], [disc.ident()], [enum_data_args...]) {
            [yul::Statement::Expression(provider.ptr_store(db, ptr.expr(), disc.expr(), make_ptr(db, disc_ty, is_sptr)))]
            [enum_data_init...]
        }
    };
    RuntimeFunction::from_statement(func_def)
}

pub(super) fn make_string_copy(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    data: &str,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let dst_ptr = YulVariable::new("dst_ptr");
    let symbol_name = literal_expression! { (format!(r#""{}""#, db.codegen_constant_string_symbol_name(data.to_string()))) };

    let func = if is_dst_storage {
        let tmp_ptr = YulVariable::new("tmp_ptr");
        let data_size = YulVariable::new("data_size");
        function_definition! {
            function [func_name.ident()]([dst_ptr.ident()]) {
                (let [tmp_ptr.ident()] := [provider.avail(db)])
                (let data_offset := dataoffset([symbol_name.clone()]))
                (let [data_size.ident()] := datasize([symbol_name]))
                (let len_slot := div([dst_ptr.expr()], 32))
                (sstore(len_slot, [data_size.expr()]))
                (datacopy([tmp_ptr.expr()], data_offset, [data_size.expr()]))
                ([dst_ptr.ident()] := add([dst_ptr.expr()], 32))
                ([yul::Statement::Expression(
                    provider.ptr_copy(db, tmp_ptr.expr(), dst_ptr.expr(), data_size.expr(), false, true))
                ])
            }
        }
    } else {
        function_definition! {
            function [func_name.ident()]([dst_ptr.ident()]) {
                (let data_offset := dataoffset([symbol_name.clone()]))
                (let data_size := datasize([symbol_name]))
                (mstore([dst_ptr.expr()], data_size))
                ([dst_ptr.ident()] := add([dst_ptr.expr()], 32))
                (datacopy([dst_ptr.expr()], data_offset, data_size))
            }
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_string_construct(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    data: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let ptr_size = YulVariable::new("ptr_size");
    let string_ptr = YulVariable::new("string_ptr");

    let func = function_definition! {
        function [func_name.ident()]([ptr_size.ident()]) -> [string_ptr.ident()] {
            ([string_ptr.ident()] := [provider.alloc(db, ptr_size.expr())])
            ([yul::Statement::Expression(provider.string_copy(db, string_ptr.expr(), data, false))])
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_map_value_ptr_with_primitive_key(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    key_ty: TypeId,
) -> RuntimeFunction {
    debug_assert!(key_ty.is_primitive(db.upcast()));
    let scratch_space = literal_expression! {(HASH_SCRATCH_SPACE_START)};
    let scratch_size = literal_expression! {(HASH_SCRATCH_SPACE_SIZE)};
    let func_name = YulVariable::new(func_name);
    let map_ptr = YulVariable::new("map_ptr");
    let key = YulVariable::new("key");
    let yul_primitive_type = yul_primitive_type(db);

    let mask = BitMask::new(1).not();

    let func = function_definition! {
        function [func_name.ident()]([map_ptr.ident()], [key.ident()]) -> ret {
        ([yul::Statement::Expression(provider.ptr_store(
            db,
            scratch_space.clone(),
            key.expr(),
            yul_primitive_type.make_mptr(db.upcast()),
        ))])
        ([yul::Statement::Expression(provider.ptr_store(
            db,
            expression!(add([scratch_space.clone()], 32)),
            map_ptr.expr(),
            yul_primitive_type.make_mptr(db.upcast()),
        ))])
        (ret := and([mask.as_expr()], (keccak256([scratch_space], [scratch_size]))))
    }};

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_map_value_ptr_with_ptr_key(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    key_ty: TypeId,
) -> RuntimeFunction {
    debug_assert!(key_ty.is_ptr(db.upcast()));

    let func_name = YulVariable::new(func_name);
    let size = literal_expression! {(key_ty.deref(db.upcast()).size_of(db.upcast(), SLOT_SIZE))};
    let map_ptr = YulVariable::new("map_ptr");
    let key = YulVariable::new("key");

    let key_hash = expression! { keccak256([key.expr()], [size]) };
    let u256_ty = yul_primitive_type(db);
    let def = function_definition! {
        function [func_name.ident()]([map_ptr.ident()], [key.ident()]) -> ret {
            (ret := [provider.map_value_ptr(db, map_ptr.expr(), key_hash, u256_ty)])
        }
    };
    RuntimeFunction::from_statement(def)
}
