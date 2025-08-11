fn main() {
    use fe_language_server::functionality::goto::goto_definition_with_hir_synthesis;
    use fe_driver::DriverDataBase;
    use fe_hir::lower::map_file_to_mod;
    use fe_common::InputDb;
    use url::Url;
    use parser::TextSize;
    
    let mut db = DriverDataBase::default();
    let content = std::fs::read_to_string("test_files/goto_debug.fe").unwrap();
    
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(std::env::current_dir().unwrap().join("test_files/goto_debug.fe")).unwrap(),
        Some(content.clone()),
    );
    let top_mod = map_file_to_mod(&db, file);
    
    // Test cursor at position 73 (one of the found cursors)
    let cursor = TextSize::from(73);
    
    // Find what character is at position 73
    let chars: Vec<char> = content.chars().collect();
    if cursor < TextSize::from(content.len() as u32) {
        println!("Character at cursor 73: '{}'", chars[73]);
        
        // Show context around cursor
        let start = 73.saturating_sub(10);
        let end = (73 + 10).min(chars.len());
        let context: String = chars[start..end].iter().collect();
        println!("Context: ...{}...", context);
    }
    
    println!("\nTrying goto at cursor position {}", cursor);
    match goto_definition_with_hir_synthesis(&db, top_mod, cursor) {
        Ok(scopes) => {
            println!("Found {} scopes", scopes.len());
            for scope in scopes {
                if let Some(path) = scope.pretty_path(&db) {
                    println!("  - {}", path);
                }
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}