use std::rc::Rc;

use fe_analyzer::display::Displayable;
use fe_analyzer::namespace::items as analyzer_items;
use fe_analyzer::namespace::items::Item;

use smol_str::SmolStr;

use crate::{
    db::MirDb,
    ir::{self, function::Linkage, types::TypeParamDef, FunctionSignature, TypeId, TypeKind},
    lower::function::{lower_func_body, lower_func_signature},
};

pub fn mir_lowered_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionSigId,
) -> ir::FunctionSigId {
    lower_func_signature(db, analyzer_func)
}

pub fn mir_lowered_func_body(
    db: &dyn MirDb,
    func: analyzer_items::FunctionId,
) -> Rc<ir::FunctionBody> {
    lower_func_body(db, func)
}

impl ir::FunctionSigId {
    pub fn data(self, db: &dyn MirDb) -> Rc<FunctionSignature> {
        db.lookup_mir_intern_function(self)
    }

    pub fn return_type(self, db: &dyn MirDb) -> Option<TypeId> {
        self.data(db).return_type
    }

    pub fn linkage(self, db: &dyn MirDb) -> Linkage {
        self.data(db).linkage
    }

    pub fn type_params(self, db: &dyn MirDb) -> Vec<TypeParamDef> {
        self.data(db)
            .params
            .iter()
            .filter_map(|param| match &param.ty.data(db).kind {
                TypeKind::TypeParam(param) => Some(param.clone()),
                _ => None,
            })
            .collect()
    }

    pub fn is_generic(self, db: &dyn MirDb) -> bool {
        self.data(db)
            .params
            .iter()
            .any(|param| param.ty.is_type_param(db))
    }

    pub fn analyzer_sig(self, db: &dyn MirDb) -> analyzer_items::FunctionSigId {
        self.data(db).analyzer_id
    }

    pub fn module(self, db: &dyn MirDb) -> analyzer_items::ModuleId {
        self.data(db).module_id
    }

    pub fn ingot(self, db: &dyn MirDb) -> analyzer_items::IngotId {
        self.analyzer_sig(db).ingot(db.upcast())
    }

    pub fn is_contract_init(self, db: &dyn MirDb) -> bool {
        self.analyzer_sig(db).is_constructor(db.upcast())
    }

    pub fn name(self, db: &dyn MirDb) -> SmolStr {
        self.data(db).name.clone()
    }

    /// Returns `class_name::fn_name` if a function is a method else `fn_name`.
    pub fn debug_name(self, db: &dyn MirDb) -> SmolStr {
        let analyzer_func = self.analyzer_sig(db);
        let func_name = format!("{}", analyzer_func.name(db.upcast()),);

        match analyzer_func.self_item(db.upcast()) {
            Some(Item::Impl(id)) => {
                let class_name = format!(
                    "<{} as {}>",
                    id.receiver(db.upcast()).display(db.upcast()),
                    id.trait_id(db.upcast()).name(db.upcast())
                );
                format!("{}::{}", class_name, func_name).into()
            }
            Some(class) => {
                let class_name = class.name(db.upcast());
                format!("{}::{}", class_name, func_name).into()
            }
            _ => func_name.into(),
        }
    }

    pub fn returns_aggregate(self, db: &dyn MirDb) -> bool {
        self.return_type(db)
            .map(|ty| ty.is_aggregate(db))
            .unwrap_or_default()
    }
}
