use fe_mir::{
    db::MirDb,
    ir::{
        body_cursor::{BodyCursor, CursorLocation},
        inst::InstKind,
        FunctionBody, Inst, InstId,
    },
};

pub fn legalize_func_body(db: &dyn MirDb, body: &mut FunctionBody) {
    let mut cursor = BodyCursor::new_at_entry(body);

    loop {
        match cursor.loc() {
            CursorLocation::BlockTop(_) | CursorLocation::BlockBottom(_) => cursor.proceed(),
            CursorLocation::Inst(inst) => {
                legalize_inst(db, cursor.body_mut(), inst);
                cursor.proceed();
            }
            CursorLocation::NoWhere => break,
        }
    }
}

fn legalize_inst(db: &dyn MirDb, body: &mut FunctionBody, inst: InstId) {
    legalize_call(db, body, inst);
    legalize_return(db, body, inst);
    legalize_inst_result(db, body, inst);
}

// Remove zero-sized arguments from call instruction.
fn legalize_call(db: &dyn MirDb, body: &mut FunctionBody, inst: InstId) {
    let dummy_inst = Inst::nop();
    let mut call_inst = body.store.replace_inst(inst, dummy_inst);
    if let InstKind::Call { args, .. } = &mut call_inst.kind {
        args.retain(|arg| !body.store.value_ty(*arg).is_zero_sized(db));
    }

    body.store.replace_inst(inst, call_inst);
}

// Remove return argument if its type is zero sized.
fn legalize_return(db: &dyn MirDb, body: &mut FunctionBody, inst: InstId) {
    if let Inst {
        kind: InstKind::Return { arg: Some(arg) },
        source,
    } = body.store.inst_data(inst)
    {
        if body.store.value_ty(*arg).is_zero_sized(db) {
            let ret_inst = Inst::new(InstKind::Return { arg: None }, source.clone());
            body.store.replace_inst(inst, ret_inst);
        }
    }
}

/// Remove instruction result if its type is zero sized.
fn legalize_inst_result(db: &dyn MirDb, body: &mut FunctionBody, inst: InstId) {
    if let Some(result) = body.store.inst_result(inst) {
        if body.store.value_ty(result).is_zero_sized(db) {
            body.store.remove_inst_result(inst)
        }
    }
}
