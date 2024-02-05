use std::rc::Rc;

use hir::hir_def::TypeId;

use crate::{
    ir::{Constant, ConstantId},
    MirDb,
};

#[salsa::tracked]
pub fn mir_lowered_constant(db: &dyn MirDb, analyzer_const: hir::hir_def::Const) -> ConstantId {
    // let name = analyzer_const.name(db.upcast());
    // let value = analyzer_const.constant_value(db.upcast()).unwrap();
    // let ty = analyzer_const.typ(db.upcast()).unwrap();
    // let module_id = analyzer_const.module(db.upcast());
    // let span = analyzer_const.span(db.upcast());
    // let id = analyzer_const.node_id(db.upcast());

    // let ty = db.mir_lowered_type(ty);

    // let constant = Constant {
    //     name,
    //     value: value.into(),
    //     ty,
    //     module_id,
    // };

    // db.mir_intern_const(constant.into())
    panic!()
}

impl ConstantId {
    // pub fn data(self, db: &dyn MirDb) -> Rc<Constant> {
    //     db.lookup_mir_intern_const(self)
    // }

    pub fn ty(self, db: &dyn MirDb) -> TypeId {
        self.data(db).ty
    }
}
