use fe_mir::ir::{
    body_cursor::{BodyCursor, CursorLocation},
    inst::InstKind,
    FunctionBody, Inst, InstId,
};

use crate::db::CodegenDb;

use super::critical_edge::CriticalEdgeSplitter;

pub fn legalize_func_body(db: &dyn CodegenDb, body: &mut FunctionBody) {
    // Remove critical edges.
    CriticalEdgeSplitter::new().run(body);

    // Remove zero-sized types usage.
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

fn legalize_inst(db: &dyn CodegenDb, body: &mut FunctionBody, inst: InstId) {
    legalize_inst_arg(db, body, inst);
    legalize_inst_result(db, body, inst);
}

fn legalize_inst_arg(db: &dyn CodegenDb, body: &mut FunctionBody, inst_id: InstId) {
    // Replace inst with dummy inst to avoid borrow checker complaining.
    let dummy_inst = Inst::nop();
    let mut inst = body.store.replace_inst(inst_id, dummy_inst);

    match &mut inst.kind {
        InstKind::AggregateConstruct { args, .. } | InstKind::Call { args, .. } => {
            args.retain(|arg| !body.store.value_ty(*arg).is_zero_sized(db.upcast()));
        }

        InstKind::Return { arg } => {
            if arg
                .map(|arg| body.store.value_ty(arg).is_zero_sized(db.upcast()))
                .unwrap_or(false)
            {
                *arg = None;
            }
        }

        _ => {}
    }

    body.store.replace_inst(inst_id, inst);
}

/// Remove instruction result if its type is zero-sized.
fn legalize_inst_result(db: &dyn CodegenDb, body: &mut FunctionBody, inst: InstId) {
    if let Some(result) = body.store.inst_result(inst) {
        if body.store.value_ty(result).is_zero_sized(db.upcast()) {
            body.store.remove_inst_result(inst)
        }
    }
}
