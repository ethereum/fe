pub mod diagnostics;

use common::InputDb;
use hir::{HirDb, LowerHirDb, SpannedHirDb};
use hir_analysis::HirAnalysisDb;

#[salsa::jar(db = DriverDb)]
pub struct Jar(diagnostics::file_line_starts);

pub trait DriverDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

impl<DB> DriverDb for DB where
    DB: Sized + salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db(common::Jar, hir::Jar, hir_analysis::Jar, Jar)]
pub struct DriverDataBase {
    storage: salsa::Storage<Self>,
}

impl HirDb for DriverDataBase {}
impl SpannedHirDb for DriverDataBase {}
impl LowerHirDb for DriverDataBase {}
impl salsa::Database for DriverDataBase {
    fn salsa_event(&self, _: salsa::Event) {
        // TODO: logger.
    }
}

impl Default for DriverDataBase {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
        };
        db.prefill();
        db
    }
}
