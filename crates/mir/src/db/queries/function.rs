use std::rc::Rc;

use fe_analyzer::namespace::items as analyzer_items;

use crate::{
    db::MirDb,
    ir::{self, FunctionSignature},
    lower::function::{lower_func_body, lower_func_signature},
};

pub fn mir_lowered_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionId,
) -> ir::FunctionId {
    lower_func_signature(db, analyzer_func)
}

pub fn mir_lowered_func_body(db: &dyn MirDb, func: ir::FunctionId) -> Rc<ir::FunctionBody> {
    lower_func_body(db, func)
}

impl ir::FunctionId {
    pub fn data(self, db: &dyn MirDb) -> Rc<FunctionSignature> {
        db.lookup_mir_intern_function(self)
    }

    pub fn analyzer_func(self, db: &dyn MirDb) -> analyzer_items::FunctionId {
        self.data(db).analyzer_func_id
    }

    pub fn module(self, db: &dyn MirDb) -> analyzer_items::ModuleId {
        let analyzer_func = self.analyzer_func(db);
        analyzer_func.module(db.upcast())
    }
}
