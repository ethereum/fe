use common::{define_input_db, InputDb};

use hir::{HirDb, LowerHirDb, SpannedHirDb};
use hir_analysis::{diagnostics::SpannedHirAnalysisDb, HirAnalysisDb};
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

define_input_db!(LanguageServerDatabase);
