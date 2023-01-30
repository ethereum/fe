use crate::{
    db::CodegenDb,
    yul::{
        runtime::{error_revert_numeric, make_ptr},
        slot_size::{yul_primitive_type, SLOT_SIZE},
        YulVariable,
    },
};

use super::{AbiSrcLocation, DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_abi::types::AbiType;
use fe_mir::ir::{self, types::ArrayDef, TypeId, TypeKind};
use yultsur::*;

pub(super) fn make_abi_encode_primitive_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let enc_size = YulVariable::new("enc_size");
    let func_def = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()]) ->  [enc_size.ident()] {
            ([src.ident()] := [provider.primitive_cast(db, src.expr(), legalized_ty)])
            ([yul::Statement::Expression(provider.ptr_store(
                db,
                dst.expr(),
                src.expr(),
                make_ptr(db, yul_primitive_type(db), is_dst_storage),
            ))])
            ([enc_size.ident()] := 32)
        }
    };

    RuntimeFunction::from_statement(func_def)
}

pub(super) fn make_abi_encode_static_array_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
) -> RuntimeFunction {
    let is_dst_storage = legalized_ty.is_sptr(db.upcast());
    let deref_ty = legalized_ty.deref(db.upcast());
    let (elem_ty, len) = match &deref_ty.data(db.upcast()).kind {
        ir::TypeKind::Array(def) => (def.elem_ty, def.len),
        _ => unreachable!(),
    };
    let elem_abi_ty = db.codegen_abi_type(elem_ty);
    let elem_ptr_ty = make_ptr(db, elem_ty, false);
    let elem_ty_size = deref_ty.array_elem_size(db.upcast(), SLOT_SIZE);

    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let enc_size = YulVariable::new("enc_size");
    let header_size = elem_abi_ty.header_size();
    let iter_count = literal_expression! {(len)};

    let func = function_definition! {
         function [func_name.ident()]([src.ident()], [dst.ident()]) -> [enc_size.ident()] {
             (for {(let i := 0)} (lt(i, [iter_count])) {(i := (add(i, 1)))}
             {

                 (pop([provider.abi_encode(db, src.expr(), dst.expr(), elem_ptr_ty, is_dst_storage)]))
                 ([src.ident()] := add([src.expr()], [literal_expression!{(elem_ty_size)}]))
                 ([dst.ident()] := add([dst.expr()], [literal_expression!{(header_size)}]))
             })
             ([enc_size.ident()] := [literal_expression! {(header_size * len)}])
         }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_abi_encode_dynamic_array_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
) -> RuntimeFunction {
    let is_dst_storage = legalized_ty.is_sptr(db.upcast());
    let deref_ty = legalized_ty.deref(db.upcast());
    let (elem_ty, len) = match &deref_ty.data(db.upcast()).kind {
        ir::TypeKind::Array(def) => (def.elem_ty, def.len),
        _ => unreachable!(),
    };
    let elem_header_size = 32;
    let total_header_size = elem_header_size * len;
    let elem_ptr_ty = make_ptr(db, elem_ty, false);
    let elem_ty_size = deref_ty.array_elem_size(db.upcast(), SLOT_SIZE);
    let header_ty = make_ptr(db, yul_primitive_type(db), is_dst_storage);

    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let header_ptr = YulVariable::new("header_ptr");
    let data_ptr = YulVariable::new("data_ptr");
    let enc_size = YulVariable::new("enc_size");
    let iter_count = literal_expression! {(len)};

    let func = function_definition! {
         function [func_name.ident()]([src.ident()], [dst.ident()]) -> [enc_size.ident()] {
             (let [header_ptr.ident()] := [dst.expr()])
             (let [data_ptr.ident()] := add([dst.expr()], [literal_expression!{(total_header_size)}]))
             ([enc_size.ident()] := [literal_expression!{(total_header_size)}])
             (for {(let i := 0)} (lt(i, [iter_count])) {(i := (add(i, 1)))}
             {

                ([yul::Statement::Expression(provider.ptr_store(db, header_ptr.expr(), enc_size.expr(), header_ty))])
                ([enc_size.ident()] := add([provider.abi_encode(db, src.expr(), data_ptr.expr(), elem_ptr_ty, is_dst_storage)], [enc_size.expr()]))
                ([header_ptr.ident()] := add([header_ptr.expr()], [literal_expression!{(elem_header_size)}]))
                ([data_ptr.ident()] := add([dst.expr()], [enc_size.expr()]))
                ([src.ident()] := add([src.expr()], [literal_expression!{(elem_ty_size)}]))
             })
         }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_abi_encode_static_aggregate_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let deref_ty = legalized_ty.deref(db.upcast());
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let enc_size = YulVariable::new("enc_size");
    let field_enc_size = YulVariable::new("field_enc_size");
    let mut body = vec![
        statement! {[enc_size.ident()] := 0 },
        statement! {let [field_enc_size.ident()] := 0 },
    ];
    let field_num = deref_ty.aggregate_field_num(db.upcast());

    for idx in 0..field_num {
        let field_ty = deref_ty.projection_ty_imm(db.upcast(), idx);
        let field_ty_ptr = make_ptr(db, field_ty, false);
        let field_offset = deref_ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE);
        let src_offset = expression! { add([src.expr()], [literal_expression!{(field_offset)}]) };
        body.push(statement!{
            [field_enc_size.ident()] := [provider.abi_encode(db, src_offset, dst.expr(), field_ty_ptr, is_dst_storage)]
        });
        body.push(statement! {
            [enc_size.ident()] := add([enc_size.expr()], [field_enc_size.expr()])
        });

        if idx < field_num - 1 {
            body.push(assignment! {[dst.ident()] :=  add([dst.expr()], [field_enc_size.expr()])});
        }
    }

    let func_def = yul::FunctionDefinition {
        name: func_name.ident(),
        parameters: vec![src.ident(), dst.ident()],
        returns: vec![enc_size.ident()],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

pub(super) fn make_abi_encode_dynamic_aggregate_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let is_src_storage = legalized_ty.is_sptr(db.upcast());
    let deref_ty = legalized_ty.deref(db.upcast());
    let field_num = deref_ty.aggregate_field_num(db.upcast());

    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let header_ptr = YulVariable::new("header_ptr");
    let enc_size = YulVariable::new("enc_size");
    let data_ptr = YulVariable::new("data_ptr");

    let total_header_size = literal_expression! { ((0..field_num).fold(0, |acc, idx| {
        let ty = deref_ty.projection_ty_imm(db.upcast(), idx);
        acc + db.codegen_abi_type(ty).header_size()
    })) };
    let mut body = statements! {
        (let [header_ptr.ident()] := [dst.expr()])
        ([enc_size.ident()] := [total_header_size])
        (let [data_ptr.ident()] := add([dst.expr()], [enc_size.expr()]))
    };

    for idx in 0..field_num {
        let field_ty = deref_ty.projection_ty_imm(db.upcast(), idx);
        let field_abi_ty = db.codegen_abi_type(field_ty);
        let field_offset =
            literal_expression! { (deref_ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE)) };
        let field_ptr = expression! { add([src.expr()], [field_offset]) };
        let field_ptr_ty = make_ptr(db, field_ty, is_src_storage);

        let stmts = if field_abi_ty.is_static() {
            statements! {
                (pop([provider.abi_encode(db, field_ptr, header_ptr.expr(), field_ptr_ty, is_dst_storage)]))
                ([header_ptr.ident()] := add([header_ptr.expr()], [literal_expression! {(field_abi_ty.header_size())}]))
            }
        } else {
            let header_ty = make_ptr(db, yul_primitive_type(db), is_dst_storage);
            statements! {
               ([yul::Statement::Expression(provider.ptr_store(db, header_ptr.expr(), enc_size.expr(), header_ty))])
               ([enc_size.ident()] := add([provider.abi_encode(db, field_ptr, data_ptr.expr(), field_ptr_ty, is_dst_storage)], [enc_size.expr()]))
               ([header_ptr.ident()] := add([header_ptr.expr()], 32))
               ([data_ptr.ident()] := add([dst.expr()], [enc_size.expr()]))
            }
        };
        body.extend_from_slice(&stmts);
    }

    let func_def = yul::FunctionDefinition {
        name: func_name.ident(),
        parameters: vec![src.ident(), dst.ident()],
        returns: vec![enc_size.ident()],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

pub(super) fn make_abi_encode_string_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let string_len = YulVariable::new("string_len");
    let enc_size = YulVariable::new("enc_size");

    let func_def = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()]) -> [enc_size.ident()] {
            (let [string_len.ident()] := mload([src.expr()]))
            (let data_size := add(32, [string_len.expr()]))
            ([enc_size.ident()] := mul((div((add(data_size, 31)), 32)), 32))
            (let padding_word_ptr := add([dst.expr()], (sub([enc_size.expr()], 32))))
            (mstore(padding_word_ptr, 0))
            ([yul::Statement::Expression(provider.ptr_copy(db, src.expr(), dst.expr(), literal_expression!{data_size}, false, is_dst_storage))])
        }
    };
    RuntimeFunction::from_statement(func_def)
}

pub(super) fn make_abi_encode_bytes_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    len: usize,
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let dst = YulVariable::new("dst");
    let enc_size = YulVariable::new("enc_size");
    let dst_len_ty = make_ptr(db, yul_primitive_type(db), is_dst_storage);

    let func_def = function_definition! {
        function [func_name.ident()]([src.ident()], [dst.ident()]) -> [enc_size.ident()] {
            ([enc_size.ident()] := [literal_expression!{ (ceil_32(32 + len)) }])
            (if (gt([enc_size.expr()], 0)) {
                (let padding_word_ptr := add([dst.expr()], (sub([enc_size.expr()], 32))))
                (mstore(padding_word_ptr, 0))
            })
            ([yul::Statement::Expression(provider.ptr_store(db, dst.expr(), literal_expression!{ (len) }, dst_len_ty))])
            ([dst.ident()] := add(32, [dst.expr()]))
            ([yul::Statement::Expression(provider.ptr_copy(db, src.expr(), dst.expr(), literal_expression!{(len)}, false, is_dst_storage))])
        }
    };
    RuntimeFunction::from_statement(func_def)
}

pub(super) fn make_abi_encode_seq(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    value_tys: &[TypeId],
    is_dst_storage: bool,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let value_num = value_tys.len();
    let abi_tys: Vec<_> = value_tys
        .iter()
        .map(|ty| db.codegen_abi_type(ty.deref(db.upcast())))
        .collect();
    let dst = YulVariable::new("dst");
    let header_ptr = YulVariable::new("header_ptr");
    let enc_size = YulVariable::new("enc_size");
    let data_ptr = YulVariable::new("data_ptr");
    let values: Vec<_> = (0..value_num)
        .map(|idx| YulVariable::new(format!("value{idx}")))
        .collect();

    let total_header_size =
        literal_expression! { (abi_tys.iter().fold(0, |acc, ty| acc + ty.header_size())) };
    let mut body = statements! {
        (let [header_ptr.ident()] := [dst.expr()])
        ([enc_size.ident()] := [total_header_size])
        (let [data_ptr.ident()] := add([dst.expr()], [enc_size.expr()]))
    };

    for i in 0..value_num {
        let ty = value_tys[i];
        let abi_ty = &abi_tys[i];
        let value = &values[i];
        let stmts = if abi_ty.is_static() {
            statements! {
                (pop([provider.abi_encode(db, value.expr(), header_ptr.expr(), ty, is_dst_storage)]))
                ([header_ptr.ident()] := add([header_ptr.expr()], [literal_expression!{ (abi_ty.header_size()) }]))
            }
        } else {
            let header_ty = make_ptr(db, yul_primitive_type(db), is_dst_storage);
            statements! {
               ([yul::Statement::Expression(provider.ptr_store(db, header_ptr.expr(), enc_size.expr(), header_ty))])
               ([enc_size.ident()] := add([provider.abi_encode(db, value.expr(), data_ptr.expr(), ty, is_dst_storage)], [enc_size.expr()]))
               ([header_ptr.ident()] := add([header_ptr.expr()], 32))
               ([data_ptr.ident()] := add([dst.expr()], [enc_size.expr()]))
            }
        };
        body.extend_from_slice(&stmts);
    }

    let mut parameters = vec![dst.ident()];
    for value in values {
        parameters.push(value.ident());
    }

    let func_def = yul::FunctionDefinition {
        name: func_name.ident(),
        parameters,
        returns: vec![enc_size.ident()],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

pub(super) fn make_abi_decode(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    types: &[TypeId],
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let header_size = types
        .iter()
        .fold(0, |acc, ty| acc + db.codegen_abi_type(*ty).header_size());
    let src = YulVariable::new("$src");
    let enc_size = YulVariable::new("$enc_size");
    let header_ptr = YulVariable::new("header_ptr");
    let data_offset = YulVariable::new("data_offset");
    let tmp_offset = YulVariable::new("tmp_offset");
    let returns: Vec<_> = (0..types.len())
        .map(|i| YulVariable::new(format!("$ret{i}")))
        .collect();

    let abi_enc_size = abi_enc_size(db, types);
    let size_check = match abi_enc_size {
        AbiEncodingSize::Static(size) => statements! {
                (if (iszero((eq([enc_size.expr()], [literal_expression!{(size)}]))))
        {             [revert_with_invalid_abi_data(provider, db)]
                })
            },
        AbiEncodingSize::Bounded { min, max } => statements! {
            (if (or(
                (lt([enc_size.expr()], [literal_expression!{(min)}])),
                (gt([enc_size.expr()], [literal_expression!{(max)}]))
            )) {
                [revert_with_invalid_abi_data(provider, db)]
            })
        },
    };

    let mut body = statements! {
        (let [header_ptr.ident()] := [src.expr()])
        (let [data_offset.ident()] :=  [literal_expression!{ (header_size) }])
        (let [tmp_offset.ident()] := 0)
    };
    for i in 0..returns.len() {
        let ret_value = &returns[i];
        let field_ty = types[i];
        let field_abi_ty = db.codegen_abi_type(field_ty.deref(db.upcast()));
        if field_abi_ty.is_static() {
            body.push(statement!{ [ret_value.ident()] := [provider.abi_decode_static(db, header_ptr.expr(), field_ty, abi_loc)] });
        } else {
            let identifiers = identifiers! {
                [ret_value.ident()]
                [tmp_offset.ident()]
            };
            body.push(yul::Statement::Assignment(yul::Assignment {
                identifiers,
                expression: provider.abi_decode_dynamic(
                    db,
                    expression! {add([src.expr()], [data_offset.expr()])},
                    field_ty,
                    abi_loc,
                ),
            }));
            body.push(statement! { ([data_offset.ident()] := add([data_offset.expr()], [tmp_offset.expr()])) });
        };

        let field_header_size = literal_expression! { (field_abi_ty.header_size()) };
        body.push(
            statement! { [header_ptr.ident()] := add([header_ptr.expr()], [field_header_size]) },
        );
    }

    let offset_check = match abi_enc_size {
        AbiEncodingSize::Static(_) => vec![],
        AbiEncodingSize::Bounded { .. } => statements! {
            (if (iszero((eq([enc_size.expr()], [data_offset.expr()])))) { [revert_with_invalid_abi_data(provider, db)] })
        },
    };

    let returns: Vec<_> = returns.iter().map(YulVariable::ident).collect();
    let func_def = function_definition! {
        function [func_name.ident()]([src.ident()], [enc_size.ident()]) -> [returns...] {
            [size_check...]
            [body...]
            [offset_check...]
        }
    };
    RuntimeFunction::from_statement(func_def)
}

impl DefaultRuntimeProvider {
    fn abi_decode_static(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        ty: TypeId,
        abi_loc: AbiSrcLocation,
    ) -> yul::Expression {
        let ty = db.codegen_legalized_type(ty).deref(db.upcast());
        let abi_ty = db.codegen_abi_type(ty.deref(db.upcast()));
        debug_assert!(abi_ty.is_static());

        let func_name_postfix = match abi_loc {
            AbiSrcLocation::CallData => "calldata",
            AbiSrcLocation::Memory => "memory",
        };

        let args = vec![src];
        if ty.is_primitive(db.upcast()) {
            let name = format! {
                "$abi_decode_primitive_type_{}_from_{}",
                ty.0, func_name_postfix,
            };
            return self.create_then_call(&name, args, |provider| {
                make_abi_decode_primitive_type(provider, db, &name, ty, abi_loc)
            });
        }

        let name = format! {
            "$abi_decode_static_aggregate_type_{}_from_{}",
            ty.0, func_name_postfix,
        };
        self.create_then_call(&name, args, |provider| {
            make_abi_decode_static_aggregate_type(provider, db, &name, ty, abi_loc)
        })
    }

    fn abi_decode_dynamic(
        &mut self,
        db: &dyn CodegenDb,
        src: yul::Expression,
        ty: TypeId,
        abi_loc: AbiSrcLocation,
    ) -> yul::Expression {
        let ty = db.codegen_legalized_type(ty).deref(db.upcast());
        let abi_ty = db.codegen_abi_type(ty.deref(db.upcast()));
        debug_assert!(!abi_ty.is_static());

        let func_name_postfix = match abi_loc {
            AbiSrcLocation::CallData => "calldata",
            AbiSrcLocation::Memory => "memory",
        };

        let mut args = vec![src];
        match abi_ty {
            AbiType::String => {
                let len = match &ty.data(db.upcast()).kind {
                    TypeKind::String(len) => *len,
                    _ => unreachable!(),
                };
                args.push(literal_expression! {(len)});
                let name = format! {"$abi_decode_string_from_{func_name_postfix}"};
                self.create_then_call(&name, args, |provider| {
                    make_abi_decode_string_type(provider, db, &name, abi_loc)
                })
            }

            AbiType::Bytes => {
                let len = match &ty.data(db.upcast()).kind {
                    TypeKind::Array(ArrayDef { len, .. }) => *len,
                    _ => unreachable!(),
                };
                args.push(literal_expression! {(len)});
                let name = format! {"$abi_decode_bytes_from_{func_name_postfix}"};
                self.create_then_call(&name, args, |provider| {
                    make_abi_decode_bytes_type(provider, db, &name, abi_loc)
                })
            }

            AbiType::Array { .. } => {
                let name =
                    format! {"$abi_decode_dynamic_array_{}_from_{}", ty.0, func_name_postfix};
                self.create_then_call(&name, args, |provider| {
                    make_abi_decode_dynamic_elem_array_type(provider, db, &name, ty, abi_loc)
                })
            }

            AbiType::Tuple(_) => {
                let name =
                    format! {"$abi_decode_dynamic_aggregate_{}_from_{}", ty.0, func_name_postfix};
                self.create_then_call(&name, args, |provider| {
                    make_abi_decode_dynamic_aggregate_type(provider, db, &name, ty, abi_loc)
                })
            }

            _ => unreachable!(),
        }
    }
}

fn make_abi_decode_primitive_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    ty: TypeId,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    debug_assert! {ty.is_primitive(db.upcast())}
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let ret = YulVariable::new("ret");

    let decode = match abi_loc {
        AbiSrcLocation::CallData => {
            statement! { [ret.ident()] := calldataload([src.expr()]) }
        }
        AbiSrcLocation::Memory => {
            statement! { [ret.ident()] := mload([src.expr()]) }
        }
    };

    let ty_size_bits = ty.size_of(db.upcast(), SLOT_SIZE) * 8;
    let validation = if ty_size_bits == 256 {
        statements! {}
    } else if ty.is_signed(db.upcast()) {
        let shift_num = literal_expression! { ( ty_size_bits - 1) };
        let tmp1 = YulVariable::new("tmp1");
        let tmp2 = YulVariable::new("tmp2");
        statements! {
            (let [tmp1.ident()] := iszero((shr([shift_num.clone()], [ret.expr()]))))
            (let [tmp2.ident()] := iszero((shr([shift_num], (not([ret.expr()]))))))
            (if (iszero((or([tmp1.expr()], [tmp2.expr()])))) {
                [revert_with_invalid_abi_data(provider, db)]
            })
        }
    } else {
        let shift_num = literal_expression! { ( ty_size_bits) };
        let tmp = YulVariable::new("tmp");
        statements! {
            (let [tmp.ident()] := iszero((shr([shift_num], [ret.expr()]))))
            (if (iszero([tmp.expr()])) {
                [revert_with_invalid_abi_data(provider, db)]
            })
        }
    };

    let func = function_definition! {
            function [func_name.ident()]([src.ident()]) -> [ret.ident()] {
                ([decode])
                [validation...]
        }
    };

    RuntimeFunction::from_statement(func)
}

fn make_abi_decode_static_aggregate_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    ty: TypeId,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    debug_assert!(ty.is_aggregate(db.upcast()));

    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let ret = YulVariable::new("ret");
    let field_data = YulVariable::new("field_data");
    let type_size = literal_expression! { (ty.size_of(db.upcast(), SLOT_SIZE)) };

    let mut body = statements! {
        (let [field_data.ident()] := 0)
        ([ret.ident()] := [provider.alloc(db, type_size)])
    };

    let field_num = ty.aggregate_field_num(db.upcast());
    for idx in 0..field_num {
        let field_ty = ty.projection_ty_imm(db.upcast(), idx);
        let field_ty_size = field_ty.size_of(db.upcast(), SLOT_SIZE);
        body.push(statement! { [field_data.ident()] := [provider.abi_decode_static(db, src.expr(), field_ty, abi_loc)] });

        let dst_offset =
            literal_expression! { (ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE)) };
        let field_ty_ptr = make_ptr(db, field_ty, false);
        if field_ty.is_primitive(db.upcast()) {
            body.push(yul::Statement::Expression(provider.ptr_store(
                db,
                expression! {add([ret.expr()], [dst_offset])},
                field_data.expr(),
                field_ty_ptr,
            )));
        } else {
            body.push(yul::Statement::Expression(provider.ptr_copy(
                db,
                field_data.expr(),
                expression! {add([ret.expr()], [dst_offset])},
                literal_expression! { (field_ty_size) },
                false,
                false,
            )));
        }

        if idx < field_num - 1 {
            let abi_field_ty = db.codegen_abi_type(field_ty);
            let field_abi_ty_size = literal_expression! { (abi_field_ty.header_size()) };
            body.push(assignment! {[src.ident()] :=  add([src.expr()], [field_abi_ty_size])});
        }
    }

    let func_def = yul::FunctionDefinition {
        name: func_name.ident(),
        parameters: vec![src.ident()],
        returns: vec![ret.ident()],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

fn make_abi_decode_string_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let decoded_data = YulVariable::new("decoded_data");
    let decoded_size = YulVariable::new("decoded_size");
    let max_len = YulVariable::new("max_len");
    let string_size = YulVariable::new("string_size");
    let dst_size = YulVariable::new("dst_size");
    let end_word = YulVariable::new("end_word");
    let end_word_ptr = YulVariable::new("end_word_ptr");
    let padding_size_bits = YulVariable::new("padding_size_bits");
    let primitive_ty_ptr = make_ptr(db, yul_primitive_type(db), false);

    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [max_len.ident()]) -> [(vec![decoded_data.ident(), decoded_size.ident()])...] {
            (let string_len := [provider.abi_decode_static(db, src.expr(), primitive_ty_ptr, abi_loc)])
            (if (gt(string_len, [max_len.expr()])) { [revert_with_invalid_abi_data(provider, db)] } )
            (let [string_size.ident()] := add(string_len, 32))
            ([decoded_size.ident()] := mul((div((add([string_size.expr()], 31)), 32)), 32))
            (let [end_word_ptr.ident()] := sub((add([src.expr()], [decoded_size.expr()])), 32))
            (let [end_word.ident()] := [provider.abi_decode_static(db, end_word_ptr.expr(), primitive_ty_ptr, abi_loc)])
            (let [padding_size_bits.ident()] := mul((sub([decoded_size.expr()], [string_size.expr()])), 8))
            [(check_right_padding(provider, db, end_word.expr(), padding_size_bits.expr()))...]
            (let [dst_size.ident()] := add([max_len.expr()], 32))
            ([decoded_data.ident()] := [provider.alloc(db, dst_size.expr())])
            ([ptr_copy_decode(provider, db, src.expr(), decoded_data.expr(), string_size.expr(), abi_loc)])
        }
    };

    RuntimeFunction::from_statement(func)
}

fn make_abi_decode_bytes_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let decoded_data = YulVariable::new("decoded_data");
    let decoded_size = YulVariable::new("decoded_size");
    let max_len = YulVariable::new("max_len");
    let bytes_size = YulVariable::new("bytes_size");
    let end_word = YulVariable::new("end_word");
    let end_word_ptr = YulVariable::new("end_word_ptr");
    let padding_size_bits = YulVariable::new("padding_size_bits");
    let primitive_ty_ptr = make_ptr(db, yul_primitive_type(db), false);

    let func = function_definition! {
        function [func_name.ident()]([src.ident()], [max_len.ident()]) -> [(vec![decoded_data.ident(),decoded_size.ident()])...] {
            (let [bytes_size.ident()] := [provider.abi_decode_static(db, src.expr(), primitive_ty_ptr, abi_loc)])
            (if (iszero((eq([bytes_size.expr()], [max_len.expr()])))) { [revert_with_invalid_abi_data(provider, db)] } )
            ([src.ident()] := add([src.expr()], 32))
            (let padded_data_size := mul((div((add([bytes_size.expr()], 31)), 32)), 32))
            ([decoded_size.ident()] := add(padded_data_size, 32))
            (let [end_word_ptr.ident()] := sub((add([src.expr()], padded_data_size)), 32))
            (let [end_word.ident()] := [provider.abi_decode_static(db, end_word_ptr.expr(), primitive_ty_ptr, abi_loc)])
            (let [padding_size_bits.ident()] := mul((sub(padded_data_size, [bytes_size.expr()])), 8))
            [(check_right_padding(provider, db, end_word.expr(), padding_size_bits.expr()))...]
            ([decoded_data.ident()] := [provider.alloc(db, max_len.expr())])
            ([ptr_copy_decode(provider, db, src.expr(), decoded_data.expr(), bytes_size.expr(), abi_loc)])
        }
    };

    RuntimeFunction::from_statement(func)
}

fn make_abi_decode_dynamic_elem_array_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    let deref_ty = legalized_ty.deref(db.upcast());
    let (elem_ty, len) = match &deref_ty.data(db.upcast()).kind {
        ir::TypeKind::Array(def) => (def.elem_ty, def.len),
        _ => unreachable!(),
    };
    let elem_ty_size = literal_expression! { (deref_ty.array_elem_size(db.upcast(), SLOT_SIZE)) };
    let total_header_size = literal_expression! { (32 * len) };
    let iter_count = literal_expression! { (len) };
    let ret_size = literal_expression! { (deref_ty.size_of(db.upcast(), SLOT_SIZE)) };

    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let header_ptr = YulVariable::new("header_ptr");
    let data_ptr = YulVariable::new("data_ptr");
    let decoded_data = YulVariable::new("decoded_data");
    let decoded_size = YulVariable::new("decoded_size");
    let decoded_size_tmp = YulVariable::new("decoded_size_tmp");
    let ret_elem_ptr = YulVariable::new("ret_elem_ptr");
    let elem_data = YulVariable::new("elem_data");

    let func = function_definition! {
        function [func_name.ident()]([src.ident()]) -> [decoded_data.ident()], [decoded_size.ident()] {
            ([decoded_data.ident()] := [provider.alloc(db, ret_size)])
            ([decoded_size.ident()] := [total_header_size])
            (let [decoded_size_tmp.ident()] := 0)
            (let [header_ptr.ident()] := [src.expr()])
            (let [data_ptr.ident()] := 0)
            (let [elem_data.ident()] := 0)
            (let [ret_elem_ptr.ident()] := [decoded_data.expr()])

            (for {(let i := 0)} (lt(i, [iter_count])) {(i := (add(i, 1)))}
             {
                 ([data_ptr.ident()] := add([src.expr()], [provider.abi_decode_static(db, header_ptr.expr(), yul_primitive_type(db), abi_loc)]))
                 ([assignment! {[elem_data.ident()], [decoded_size_tmp.ident()] := [provider.abi_decode_dynamic(db, data_ptr.expr(), elem_ty, abi_loc)] }])
                 ([decoded_size.ident()] := add([decoded_size.expr()], [decoded_size_tmp.expr()]))
                 ([yul::Statement::Expression(provider.ptr_copy(db, elem_data.expr(), ret_elem_ptr.expr(), elem_ty_size.clone(), false, false))])
                 ([header_ptr.ident()] := add([header_ptr.expr()], 32))
                 ([ret_elem_ptr.ident()] := add([ret_elem_ptr.expr()], [elem_ty_size]))
             })
        }
    };

    RuntimeFunction::from_statement(func)
}

fn make_abi_decode_dynamic_aggregate_type(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
    abi_loc: AbiSrcLocation,
) -> RuntimeFunction {
    let deref_ty = legalized_ty.deref(db.upcast());
    let type_size = literal_expression! { (deref_ty.size_of(db.upcast(), SLOT_SIZE)) };

    let func_name = YulVariable::new(func_name);
    let src = YulVariable::new("src");
    let header_ptr = YulVariable::new("header_ptr");
    let data_offset = YulVariable::new("data_offset");
    let decoded_data = YulVariable::new("decoded_data");
    let decoded_size = YulVariable::new("decoded_size");
    let decoded_size_tmp = YulVariable::new("decoded_size_tmp");
    let ret_field_ptr = YulVariable::new("ret_field_ptr");
    let field_data = YulVariable::new("field_data");

    let mut body = statements! {
            ([decoded_data.ident()] := [provider.alloc(db, type_size)])
            ([decoded_size.ident()] := 0)
            (let [decoded_size_tmp.ident()] := 0)
            (let [header_ptr.ident()] := [src.expr()])
            (let [data_offset.ident()] := 0)
            (let [field_data.ident()] := 0)
            (let [ret_field_ptr.ident()] := 0)
    };

    for i in 0..deref_ty.aggregate_field_num(db.upcast()) {
        let field_ty = deref_ty.projection_ty_imm(db.upcast(), i);
        let field_size = field_ty.size_of(db.upcast(), SLOT_SIZE);
        let field_abi_ty = db.codegen_abi_type(field_ty);
        let field_offset = deref_ty.aggregate_elem_offset(db.upcast(), i, SLOT_SIZE);

        let decode_data = if field_abi_ty.is_static() {
            statements! {
                ([field_data.ident()] := [provider.abi_decode_static(db, header_ptr.expr(), field_ty, abi_loc)])
                ([decoded_size_tmp.ident()] := [literal_expression!{ (field_abi_ty.header_size()) }])
            }
        } else {
            statements! {
                ([data_offset.ident()] := [provider.abi_decode_static(db, header_ptr.expr(), yul_primitive_type(db), abi_loc)])
                ([assignment! {
                    [field_data.ident()], [decoded_size_tmp.ident()] :=
                    [provider.abi_decode_dynamic(
                        db,
                        expression!{ add([src.expr()], [data_offset.expr()]) },
                        field_ty,
                        abi_loc
                    )]
                }])
                ([decoded_size_tmp.ident()] := add([decoded_size_tmp.expr()], 32))
            }
        };
        body.extend_from_slice(&decode_data);
        body.push(assignment!{ [decoded_size.ident()] := add([decoded_size.expr()], [decoded_size_tmp.expr()]) });

        body.push(assignment! { [ret_field_ptr.ident()] := add([decoded_data.expr()], [literal_expression!{ (field_offset) }])});
        let copy_to_ret = if field_ty.is_primitive(db.upcast()) {
            let field_ptr_ty = make_ptr(db, field_ty, false);
            yul::Statement::Expression(provider.ptr_store(
                db,
                ret_field_ptr.expr(),
                field_data.expr(),
                field_ptr_ty,
            ))
        } else {
            yul::Statement::Expression(provider.ptr_copy(
                db,
                field_data.expr(),
                ret_field_ptr.expr(),
                literal_expression! { (field_size) },
                false,
                false,
            ))
        };
        body.push(copy_to_ret);

        let header_size = literal_expression! { (field_abi_ty.header_size()) };
        body.push(statement! {
            [header_ptr.ident()] := add([header_ptr.expr()], [header_size])
        });
    }

    let func_def = yul::FunctionDefinition {
        name: func_name.ident(),
        parameters: vec![src.ident()],
        returns: vec![decoded_data.ident(), decoded_size.ident()],
        block: yul::Block { statements: body },
    };

    RuntimeFunction(func_def)
}

enum AbiEncodingSize {
    Static(usize),
    Bounded { min: usize, max: usize },
}

fn abi_enc_size(db: &dyn CodegenDb, types: &[TypeId]) -> AbiEncodingSize {
    let mut min = 0;
    let mut max = 0;
    for &ty in types {
        let legalized_ty = db.codegen_legalized_type(ty);
        min += db.codegen_abi_type_minimum_size(legalized_ty);
        max += db.codegen_abi_type_maximum_size(legalized_ty);
    }

    if min == max {
        AbiEncodingSize::Static(min)
    } else {
        AbiEncodingSize::Bounded { min, max }
    }
}

fn revert_with_invalid_abi_data(
    provider: &mut dyn RuntimeProvider,
    db: &dyn CodegenDb,
) -> yul::Statement {
    const ERROR_INVALID_ABI_DATA: usize = 0x103;
    let error_code = literal_expression! { (ERROR_INVALID_ABI_DATA) };
    error_revert_numeric(provider, db, error_code)
}

fn check_right_padding(
    provider: &mut dyn RuntimeProvider,
    db: &dyn CodegenDb,
    val: yul::Expression,
    size_bits: yul::Expression,
) -> Vec<yul::Statement> {
    statements! {
        (let bits_shifted := sub(256, [size_bits]))
        (let is_ok := iszero((shl(bits_shifted, [val]))))
        (if (iszero((is_ok))) {
            [revert_with_invalid_abi_data(provider, db)]
        })
    }
}

fn ptr_copy_decode(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    src: yul::Expression,
    dst: yul::Expression,
    len: yul::Expression,
    loc: AbiSrcLocation,
) -> yul::Statement {
    match loc {
        AbiSrcLocation::CallData => {
            statement! { calldatacopy([dst], [src], [len]) }
        }
        AbiSrcLocation::Memory => {
            yul::Statement::Expression(provider.ptr_copy(db, src, dst, len, false, false))
        }
    }
}

fn ceil_32(len: usize) -> usize {
    ((len + 31) / 32) * 32
}
