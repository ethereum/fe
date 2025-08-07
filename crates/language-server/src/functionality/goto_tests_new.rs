// New test approach that doesn't rely on visitor infrastructure
use super::*;
use std::collections::BTreeMap;

/// Test data structure containing expected goto results for specific positions
struct GotoTestCase {
    position: (usize, usize), // (line, column)
    expected_path: Option<&'static str>,
}

/// Run goto tests for a specific file with predefined test cases
fn run_goto_test_cases(
    db: &DriverDataBase,
    top_mod: TopLevelMod,
    content: &str,
    test_cases: &[GotoTestCase],
) -> String {
    let mut results = Vec::new();
    
    for test_case in test_cases {
        let pos = async_lsp::lsp_types::Position::new(
            test_case.position.0 as u32,
            test_case.position.1 as u32,
        );
        let cursor = crate::util::to_offset_from_position(pos, content);
        
        match goto_definition_with_hir_synthesis(db, top_mod, cursor) {
            Ok(scopes) => {
                let paths: Vec<String> = scopes
                    .iter()
                    .flat_map(|s| s.pretty_path(db))
                    .collect();
                
                let path_str = if paths.is_empty() {
                    "<no definition>".to_string()
                } else {
                    paths.join(", ")
                };
                
                results.push(format!(
                    "cursor position ({}, {}): {}",
                    test_case.position.0,
                    test_case.position.1,
                    path_str
                ));
            }
            Err(e) => {
                results.push(format!(
                    "cursor position ({}, {}): error - {:?}",
                    test_case.position.0,
                    test_case.position.1,
                    e
                ));
            }
        }
    }
    
    // Format output similar to original snapshots
    let lines: Vec<String> = content
        .lines()
        .enumerate()
        .map(|(i, line)| format!("{}: {}", i, line))
        .collect();
    
    format!("{}\n---\n{}", lines.join("\n"), results.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use dir_test::{dir_test, Fixture};
    use test_utils::snap_test;
    use url::Url;
    use driver::DriverDataBase;
    use common::InputDb;
    use hir::lower::map_file_to_mod;
    
    // Define test cases for goto_debug.fe
    fn get_goto_debug_test_cases() -> Vec<GotoTestCase> {
        vec![
            // Test "Point" type references
            GotoTestCase { position: (6, 12), expected_path: Some("goto_debug::Point") }, // Point in "let p = Point"
            GotoTestCase { position: (7, 14), expected_path: Some("goto_debug::Point::x") }, // x in "p.x"
        ]
    }
    
    #[test]
    fn test_goto_debug_manual() {
        let content = std::fs::read_to_string("test_files/goto_debug.fe").unwrap();
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(std::env::current_dir().unwrap().join("test_files/goto_debug.fe")).unwrap(),
            Some(content.clone()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        let snapshot = run_goto_test_cases(&db, top_mod, &content, &get_goto_debug_test_cases());
        
        // For now just print the output to see what we get
        println!("=== GOTO DEBUG TEST OUTPUT ===");
        println!("{}", snapshot);
        println!("=== END OUTPUT ===");
    }
}