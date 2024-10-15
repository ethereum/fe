use common::InputDb;

use hir::{HirDb, LowerHirDb, SpannedHirDb};
use hir_analysis::HirAnalysisDb;
use salsa::{ParallelDatabase, Snapshot};

#[salsa::jar(db = LanguageServerDb)]
pub struct Jar(crate::functionality::diagnostics::file_line_starts);

pub trait LanguageServerDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

impl<DB> LanguageServerDb for DB where
    DB: Sized + salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db(
    common::Jar,
    hir::Jar,
    hir::LowerJar,
    hir::SpannedJar,
    hir_analysis::Jar,
    Jar
)]
#[derive(Default)]
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for LanguageServerDatabase {
    fn salsa_event(&self, _: salsa::Event) {}
}

impl ParallelDatabase for LanguageServerDatabase {
    fn snapshot(&self) -> Snapshot<Self> {
        Snapshot::new(LanguageServerDatabase {
            storage: self.storage.snapshot(),
        })
    }
}
