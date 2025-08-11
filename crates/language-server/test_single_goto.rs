fn main() {
    use fe_language_server::functionality::goto::goto_definition_with_hir_synthesis;
    use fe_driver::DriverDataBase;
    use fe_hir::lower::map_file_to_mod;
    use fe_common::InputDb;
    use url::Url;
    use fe_language_server::util::to_offset_from_position;
    
    let mut db = DriverDataBase::default();
    let content = r#"struct Point {
    pub x: i32,
    pub y: i32
}

fn test() {
    let p = Point { x: 1, y: 2 }
    let val = p.x
}"#;
    
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path("/tmp/test.fe").unwrap(),
        Some(content.to_string()),
    );
    let top_mod = map_file_to_mod(&db, file);
    
    // Try at position of "Point" in line 6 (let p = Point)
    let pos = async_lsp::lsp_types::Position::new(6, 12);
    let cursor = to_offset_from_position(pos, content);
    
    println!("Testing at line 6, col 12 (should be 'Point')");
    println!("Character at cursor: '{}'", content.chars().nth(cursor.into()).unwrap_or('?'));
    
    match goto_definition_with_hir_synthesis(&db, top_mod, cursor) {
        Ok(targets) => {
            println!("Found {} targets", targets.len());
            for target in targets {
                if let Some(path) = target.pretty_path(&db) {
                    println!("  - {}", path);
                }
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}