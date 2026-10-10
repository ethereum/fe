//! Associated types with parameters agree with their plain twins (the cases
//! are in `family_twin_cases.rs`), checked with the analysis passes. Each
//! program is checked in a fresh database and in one database edited from
//! program to program; both must give the same codes.

use std::cell::RefCell;
use std::panic::{AssertUnwindSafe, catch_unwind};

use common::file::File;
use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};
use salsa::Setter;

include!("family_twin_cases.rs");

/// The error codes of `file`, sorted; identical diagnostics count once.
fn codes_of(db: &HirAnalysisTestDb, file: File) -> Vec<String> {
    let (top_mod, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    let mut seen = Vec::new();
    for diag in manager.run_on_module(db, top_mod) {
        let diag = diag.to_complete(db);
        if !seen.contains(&diag) {
            seen.push(diag);
        }
    }
    let mut codes: Vec<String> = seen
        .into_iter()
        .map(|diag| diag.error_code.to_string())
        .collect();
    codes.sort();
    codes
}

thread_local! {
    static EDITED: RefCell<Option<(HirAnalysisTestDb, File)>> = const { RefCell::new(None) };
}

fn codes(src: &str) -> Vec<String> {
    let fresh = catch_unwind(|| {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("twin.fe".into(), src);
        codes_of(&db, file)
    })
    .unwrap_or_else(|_| vec!["crash".to_string()]);
    let edited = EDITED.with(|cell| {
        let mut cell = cell.borrow_mut();
        let result = catch_unwind(AssertUnwindSafe(|| {
            let (db, file) = cell.get_or_insert_with(|| {
                let mut db = HirAnalysisTestDb::default();
                let file = db.new_stand_alone("twin.fe".into(), "");
                (db, file)
            });
            file.set_text(db).to(src.to_owned());
            codes_of(db, *file)
        }));
        result.unwrap_or_else(|_| {
            *cell = None;
            vec!["crash".to_string()]
        })
    });
    if edited == fresh {
        fresh
    } else {
        let mut codes = fresh;
        codes.push(format!("edited database gives {edited:?}"));
        codes
    }
}

#[test]
fn families_agree_with_their_plain_twins() {
    check_all_twins(&codes, 8, true);
}

#[test]
fn family_rows_of_the_limit_matrix() {
    family_limit::check(&codes);
}

#[test]
fn nested_family_applications() {
    check_nested_applications(&codes);
}
