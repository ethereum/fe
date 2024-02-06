use std::rc::Rc;

use hir::hir_def::{Const, TypeId};

use crate::{
    ir::{Const, ConstId},
    MirDb,
};

#[salsa::tracked]
pub fn mir_lowered_constant(db: &dyn MirDb, hir_const: Const) -> ConstId {
    let value = hir_const.constant_value(db.as_hir_db()).unwrap();

    let constant = Const {
        value: value.into(),
        origin: hir_const,
    }
}
