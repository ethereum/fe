use fe_mir::ir::{
    body_cursor::{BodyCursor, CursorLocation},
    inst::InstKind,
    value::AssignableValue,
    FunctionBody, Inst, InstId, TypeId, TypeKind, Value, ValueId,
};

use crate::db::CodegenDb;

use super::critical_edge::CriticalEdgeSplitter;

pub fn legalize_func_body(db: &dyn CodegenDb, body: &mut FunctionBody) {
    CriticalEdgeSplitter::new().run(body);
    legalize_func_arg(db, body);

    let mut cursor = BodyCursor::new_at_entry(body);
    loop {
        match cursor.loc() {
            CursorLocation::BlockTop(_) | CursorLocation::BlockBottom(_) => cursor.proceed(),
            CursorLocation::Inst(inst) => {
                legalize_inst(db, &mut cursor, inst);
            }
            CursorLocation::NoWhere => break,
        }
    }
}

fn legalize_func_arg(db: &dyn CodegenDb, body: &mut FunctionBody) {
    for value in body.store.func_args_mut() {
        let ty = value.ty();
        if ty.is_contract(db.upcast()) {
            let slot_ptr = make_storage_ptr(db, ty);
            *value = slot_ptr;
        } else if (ty.is_aggregate(db.upcast()) || ty.is_string(db.upcast()))
            && !ty.is_zero_sized(db.upcast())
        {
            change_ty(value, ty.make_mptr(db.upcast()))
        }
    }
}

fn legalize_inst(db: &dyn CodegenDb, cursor: &mut BodyCursor, inst: InstId) {
    if legalize_unit_construct(db, cursor, inst) {
        return;
    }
    legalize_declared_ty(db, cursor.body_mut(), inst);
    legalize_inst_arg(db, cursor.body_mut(), inst);
    legalize_inst_result(db, cursor.body_mut(), inst);
    cursor.proceed();
}

fn legalize_unit_construct(db: &dyn CodegenDb, cursor: &mut BodyCursor, inst: InstId) -> bool {
    let should_remove = match &cursor.body().store.inst_data(inst).kind {
        InstKind::Declare { local } => is_value_zst(db, cursor.body(), *local),
        InstKind::AggregateConstruct { ty, .. } => ty.deref(db.upcast()).is_zero_sized(db.upcast()),
        InstKind::AggregateAccess { .. } | InstKind::MapAccess { .. } | InstKind::Cast { .. } => {
            let result_value = cursor.body().store.inst_result(inst).unwrap();
            is_lvalue_zst(db, cursor.body(), result_value)
        }

        _ => false,
    };

    if should_remove {
        cursor.remove_inst()
    }

    should_remove
}

fn legalize_declared_ty(db: &dyn CodegenDb, body: &mut FunctionBody, inst_id: InstId) {
    let value = match &body.store.inst_data(inst_id).kind {
        InstKind::Declare { local } => *local,
        _ => return,
    };

    let value_ty = body.store.value_ty(value);
    if value_ty.is_aggregate(db.upcast()) {
        let new_ty = value_ty.make_mptr(db.upcast());
        let value_data = body.store.value_data_mut(value);
        change_ty(value_data, new_ty)
    }
}

fn legalize_inst_arg(db: &dyn CodegenDb, body: &mut FunctionBody, inst_id: InstId) {
    // Replace inst with dummy inst to avoid borrow checker complaining.
    let dummy_inst = Inst::nop();
    let mut inst = body.store.replace_inst(inst_id, dummy_inst);

    for arg in inst.args() {
        let ty = body.store.value_ty(arg);
        if ty.is_string(db.upcast()) {
            let string_ptr = ty.make_mptr(db.upcast());
            change_ty(body.store.value_data_mut(arg), string_ptr)
        }
    }

    match &mut inst.kind {
        InstKind::AggregateConstruct { args, .. } => {
            args.retain(|arg| !is_value_zst(db, body, *arg));
        }

        InstKind::Call { args, .. } => {
            args.retain(|arg| !is_value_zst(db, body, *arg) && !is_value_contract(db, body, *arg))
        }

        InstKind::Return { arg } => {
            if arg.map(|arg| is_value_zst(db, body, arg)).unwrap_or(false) {
                *arg = None;
            }
        }

        InstKind::MapAccess { key: arg, .. } | InstKind::Emit { arg } => {
            let arg_ty = body.store.value_ty(*arg);
            if arg_ty.is_zero_sized(db.upcast()) {
                *arg = body.store.store_value(make_zst_ptr(db, arg_ty));
            }
        }

        InstKind::Cast { value, to, .. } => {
            if to.is_aggregate(db.upcast()) && !to.is_zero_sized(db.upcast()) {
                let value_ty = body.store.value_ty(*value);
                if value_ty.is_mptr(db.upcast()) {
                    *to = to.make_mptr(db.upcast());
                } else if value_ty.is_sptr(db.upcast()) {
                    *to = to.make_sptr(db.upcast());
                } else {
                    unreachable!()
                }
            }
        }

        _ => {}
    }

    body.store.replace_inst(inst_id, inst);
}

fn legalize_inst_result(db: &dyn CodegenDb, body: &mut FunctionBody, inst_id: InstId) {
    let result_value = if let Some(result) = body.store.inst_result(inst_id) {
        result
    } else {
        return;
    };

    if is_lvalue_zst(db, body, result_value) {
        body.store.remove_inst_result(inst_id);
        return;
    };

    let value_id = if let Some(value_id) = result_value.value_id() {
        value_id
    } else {
        return;
    };
    let result_ty = body.store.value_ty(value_id);
    let new_ty = if result_ty.is_aggregate(db.upcast()) || result_ty.is_string(db.upcast()) {
        match &body.store.inst_data(inst_id).kind {
            InstKind::AggregateAccess { value, .. } => {
                let value_ty = body.store.value_ty(*value);
                match &value_ty.data(db.upcast()).kind {
                    TypeKind::MPtr(..) => result_ty.make_mptr(db.upcast()),
                    // Note: All SPtr aggregate access results should be SPtr already
                    _ => unreachable!(),
                }
            }
            _ => result_ty.make_mptr(db.upcast()),
        }
    } else {
        return;
    };

    let value = body.store.value_data_mut(value_id);
    change_ty(value, new_ty);
}

fn change_ty(value: &mut Value, new_ty: TypeId) {
    match value {
        Value::Local(val) => val.ty = new_ty,
        Value::Immediate { ty, .. }
        | Value::Temporary { ty, .. }
        | Value::Unit { ty }
        | Value::Constant { ty, .. } => *ty = new_ty,
    }
}

fn make_storage_ptr(db: &dyn CodegenDb, ty: TypeId) -> Value {
    debug_assert!(ty.is_contract(db.upcast()));
    let ty = ty.make_sptr(db.upcast());

    Value::Immediate { imm: 0.into(), ty }
}

fn make_zst_ptr(db: &dyn CodegenDb, ty: TypeId) -> Value {
    debug_assert!(ty.is_zero_sized(db.upcast()));
    let ty = ty.make_mptr(db.upcast());

    Value::Immediate { imm: 0.into(), ty }
}

/// Returns `true` if a value has a zero sized type.
fn is_value_zst(db: &dyn CodegenDb, body: &FunctionBody, value: ValueId) -> bool {
    body.store
        .value_ty(value)
        .deref(db.upcast())
        .is_zero_sized(db.upcast())
}

fn is_value_contract(db: &dyn CodegenDb, body: &FunctionBody, value: ValueId) -> bool {
    let ty = body.store.value_ty(value);
    ty.deref(db.upcast()).is_contract(db.upcast())
}

fn is_lvalue_zst(db: &dyn CodegenDb, body: &FunctionBody, lvalue: &AssignableValue) -> bool {
    lvalue
        .ty(db.upcast(), &body.store)
        .deref(db.upcast())
        .is_zero_sized(db.upcast())
}
