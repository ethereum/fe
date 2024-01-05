use fe_hir::HirDb;

// pub mod analysis;
// pub mod graphviz;
pub mod ir;
// pub mod pretty_print;

// mod lower;

#[salsa::jar(db = MirDb)]
pub struct Jar(
    ir::BasicBlock,
    ir::BasicBlockId,
    // ir::Constant,
    // ir::ConstantId,
    // ir::FunctionBody,
    // ir::FunctionId,
    // ir::FunctionParam,
    // ir::FunctionSignature,
    // ir::Inst,
    // ir::InstId,
    // ir::Value,
    // ir::ValueId,
);

#[salsa::jar(db = LowerMirDb)]
pub struct LowerJar();

pub trait MirDb: salsa::DbWithJar<Jar> + HirDb {
    // fn prefill(&self)
    // where
    //     Self: Sized,
    // {
    //     IdentId::prefill(self)
    // }

    fn as_hir_db(&self) -> &dyn MirDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> MirDb for DB where DB: salsa::DbWithJar<Jar> + HirDb {}

pub trait LowerMirDb: salsa::DbWithJar<LowerJar> + HirDb {
    fn as_lower_hir_db(&self) -> &dyn LowerMirDb {
        <Self as salsa::DbWithJar<LowerJar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> LowerMirDb for DB where DB: salsa::DbWithJar<LowerJar> + MirDb {}
