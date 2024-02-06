use hir::HirDb;

pub mod analysis;
// pub mod graphviz;
pub mod ir;
// pub mod pretty_print;

mod lower;

#[salsa::jar(db = MirDb)]
pub struct Jar(
    // ir::Constant,
    ir::ConstId,
    // ir::FunctionBody,
    // ir::FunctionId,
    // ir::FunctionParam,
    // ir::FunctionSignature,
    // ir::Inst,
    // ir::InstId,
    // ir::Value,
    // ir::ValueId,
    // mir_intern_const,
    // mir_intern_type,
    // mir_intern_function,
    // mir_lower_module_all_functions,
    // mir_lower_contract_all_functions,
    // mir_lower_struct_all_functions,
    // mir_lower_enum_all_functions,
    // mir_lowered_type,
    lower::constant::mir_lowered_constant,
    // mir_lowered_func_signature,
    // mir_lowered_monomorphized_func_signature,
    // mir_lowered_pseudo_monomorphized_func_signature,
    // mir_lowered_func_body,
);

#[salsa::jar(db = LowerMirDb)]
pub struct LowerJar();

pub trait MirDb: salsa::DbWithJar<Jar> + HirDb {
    fn prefill(&self)
    where
        Self: Sized,
    {
        // IdentId::prefill(self)
    }

    // fn as_hir_db(&self) -> &dyn MirDb {
    //     <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    // }
}
impl<DB> MirDb for DB where DB: salsa::DbWithJar<Jar> + HirDb {}

pub trait LowerMirDb: salsa::DbWithJar<LowerJar> + HirDb {
    fn as_lower_hir_db(&self) -> &dyn LowerMirDb {
        <Self as salsa::DbWithJar<LowerJar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> LowerMirDb for DB where DB: salsa::DbWithJar<LowerJar> + MirDb {}
