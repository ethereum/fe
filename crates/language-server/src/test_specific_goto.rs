use fe_driver::DriverDataBase;
use fe_common::db::Workspace;
use hir::{HirDb, analysis::AnalysisHost};
use common::diagnostics::Cursor;
use url::Url;
use std::path::Path;

#[test] 
fn test_specific_goto_issues() {
    let mut db = DriverDataBase::default();
    let content = std::fs::read_to_string("test_files/goto_specific_issues.fe").unwrap();
    let file_path = Path::new("test_files/goto_specific_issues.fe").canonicalize().unwrap();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(&file_path).unwrap(),
        Some(content.clone()),
    );
    
    let diags = db.run_on_file(file);
    let file_data = file.data(&db);
    let ingot = file_data.ingot(&db);
    let top_mod = ingot.root_mod(&db);
    
    // Test 1: NESTED_CONST resolution (line 8, col 21 in 1-indexed, so line 7, col 20 in 0-indexed)
    println!("\n=== Test 1: Multi-segment path NESTED_CONST ===");
    let cursor_on_const = Cursor::new(file, 7, 20);
    if let Some(scopes) = fe_hir_analysis::tooling_api::get_goto_definition_scopes(&db, top_mod, cursor_on_const) {
        let paths: Vec<_> = scopes.iter().flat_map(|s| s.pretty_path(&db)).collect();
        println!("Cursor on NESTED_CONST resolved to: {:?}", paths);
        assert!(paths.iter().any(|p| p.contains("NESTED_CONST")), 
                "Expected NESTED_CONST resolution, got: {:?}", paths);
    } else {
        panic!("No definition found for NESTED_CONST");
    }
    
    // Test 2: container variable resolution (line 25, col 18 in 1-indexed, so line 24, col 17 in 0-indexed)
    println!("\n=== Test 2: Local variable 'container' ===");
    let cursor_on_container = Cursor::new(file, 24, 17);
    if let Some(scopes) = fe_hir_analysis::tooling_api::get_goto_definition_scopes(&db, top_mod, cursor_on_container) {
        let paths: Vec<_> = scopes.iter().flat_map(|s| s.pretty_path(&db)).collect();
        println!("Cursor on container resolved to: {:?}", paths);
        // Local variables might not have a pretty path, check if we got any scope
        assert!(!scopes.is_empty(), "Expected container variable resolution");
    } else {
        panic!("No definition found for container variable");
    }
}

fn main() {
    test_specific_goto_issues();
}