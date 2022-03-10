use crate::{
    db::CodegenDb,
    yul::{runtime::make_ptr, slot_size::SLOT_SIZE, YulVariable},
};

use super::{DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_mir::ir::TypeId;

use yultsur::*;

pub(super) fn make_emit(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    legalized_ty: TypeId,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let event_ptr = YulVariable::new("event_ptr");
    let deref_ty = legalized_ty.deref(db.upcast());

    let abi = db.codegen_abi_event(deref_ty);
    let mut topics = vec![literal_expression! {(format!("0x{}", abi.signature().hash_hex()))}];
    for (idx, field) in abi.inputs.iter().enumerate() {
        if !field.indexed {
            continue;
        }
        let field_ty = deref_ty.projection_ty_imm(db.upcast(), idx);
        let offset =
            literal_expression! {(deref_ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE))};
        let elem_ptr = expression! { add([event_ptr.expr()], [offset]) };
        let topic = if field_ty.is_aggregate(db.upcast()) {
            todo!()
        } else {
            let topic = provider.ptr_load(
                db,
                elem_ptr,
                make_ptr(db, field_ty, legalized_ty.is_sptr(db.upcast())),
            );
            provider.primitive_cast(db, topic, field_ty)
        };

        topics.push(topic)
    }

    let mut event_data_tys = vec![];
    let mut event_data_values = vec![];
    for (idx, field) in abi.inputs.iter().enumerate() {
        if field.indexed {
            continue;
        }

        let field_ty = deref_ty.projection_ty_imm(db.upcast(), idx);
        let field_offset =
            literal_expression! { (deref_ty.aggregate_elem_offset(db.upcast(), idx, SLOT_SIZE)) };
        event_data_tys.push(make_ptr(db, field_ty, legalized_ty.is_sptr(db.upcast())));
        event_data_values.push(expression! { add([event_ptr.expr()], [field_offset]) });
    }

    debug_assert!(topics.len() < 5);
    let log_func = identifier! { (format!("log{}", topics.len()))};

    let event_data_ptr = YulVariable::new("event_data_ptr");
    let event_enc_size = YulVariable::new("event_enc_size");
    let func = function_definition! {
        function [func_name.ident()]([event_ptr.ident()]) {
            (let [event_data_ptr.ident()] := [provider.avail(db)])
            (let [event_enc_size.ident()] := [provider.abi_encode_seq(db, &event_data_values, event_data_ptr.expr(), &event_data_tys, false )])
            ([log_func]([event_data_ptr.expr()], [event_enc_size.expr()], [topics...]))
        }
    };

    RuntimeFunction::from_statement(func)
}
