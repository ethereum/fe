mod test_db;
use std::path::Path;

use dir_test::{dir_test, Fixture};
use test_db::initialize_analysis_pass;
use test_db::HirAnalysisTestDb;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "exhaustive/*.fe"
)]
fn exhaustive_matches(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "non_exhaustive/*.fe"
)]
fn non_exhaustive_matches(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    let mut manager = initialize_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected pattern matching errors but found none"
    );
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "unreachable/*.fe"
)]
fn unreachable_patterns(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    let mut manager = initialize_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected unreachable pattern errors but found none"
    );
}
