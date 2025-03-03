use common::{impl_db_traits, InputDb};

use hir::{HirDb, LowerHirDb, SpannedHirDb};
use hir_analysis::{diagnostics::SpannedHirAnalysisDb, HirAnalysisDb};

use super::get_core::init_core_ingot;
// xxx use salsa::{ParallelDatabase, Snapshot};

#[salsa::db]
pub trait LanguageServerDb:
    salsa::Database + SpannedHirAnalysisDb + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db]
impl<DB> LanguageServerDb for DB where
    DB: Sized
        + salsa::Database
        + SpannedHirAnalysisDb
        + HirAnalysisDb
        + HirDb
        + LowerHirDb
        + SpannedHirDb
        + InputDb
{
}

#[salsa::db]
#[derive(Clone)]
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
}

impl Default for LanguageServerDatabase {
    fn default() -> Self {
        let mut db = LanguageServerDatabase {
            storage: Default::default(),
        };

        init_core_ingot(&mut db);
        db
    }
}

impl_db_traits!(
    LanguageServerDatabase,
    InputDb,
    HirDb,
    LowerHirDb,
    SpannedHirDb,
    HirAnalysisDb,
    SpannedHirAnalysisDb,
);
