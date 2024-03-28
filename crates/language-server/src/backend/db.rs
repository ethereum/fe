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
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
}

impl LanguageServerDatabase {
    pub fn as_language_server_db(&self) -> &dyn LanguageServerDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}

impl salsa::Database for LanguageServerDatabase {
    fn salsa_event(&self, _: salsa::Event) {}
}

impl Default for LanguageServerDatabase {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
        };
        db.prefill();
        db
    }
}

impl ParallelDatabase for LanguageServerDatabase {
    fn snapshot(&self) -> Snapshot<Self> {
        Snapshot::new(LanguageServerDatabase {
            storage: self.storage.snapshot(),
        })
    }
}
