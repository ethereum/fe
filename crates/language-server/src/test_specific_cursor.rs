#[cfg(test)]
mod test_specific_cursor {
    use driver::DriverDataBase;
    use fe_common::db::Workspace;
    use hir::HirDb;
    use common::diagnostics::Cursor;
    use url::Url;
    use crate::functionality::goto::get_goto_target_scopes_for_cursor;

    #[test]
    fn test_nested_const_cursor_resolution() {
        let mut db = DriverDataBase::default();
        let content = r#"mod nested {
    pub const NESTED_CONST: u32 = 100
}

fn test_nested() {
    let a = nested::NESTED_CONST
}"#;

        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(content.to_string()),
        );
        
        let _ = db.run_on_file(file);
        let file_data = file.data(&db);
        let ingot = file_data.ingot(&db);
        let top_mod = ingot.root_mod(&db);
        
        // Test cursor on "nested" (line 6, col 13)
        let cursor_on_nested = Cursor::new(file, 5, 12); // 0-indexed
        let scopes_nested = get_goto_target_scopes_for_cursor(&db, top_mod, cursor_on_nested);
        if let Some(scopes) = scopes_nested {
            let paths: Vec<_> = scopes.iter().flat_map(|s| s.pretty_path(&db)).collect();
            println!("Cursor on 'nested': {:?}", paths);
            assert!(paths.iter().any(|p| p.contains("nested") && !p.contains("NESTED_CONST")));
        }
        
        // Test cursor on "NESTED_CONST" (line 6, col 21)
        let cursor_on_const = Cursor::new(file, 5, 20); // 0-indexed
        let scopes_const = get_goto_target_scopes_for_cursor(&db, top_mod, cursor_on_const);
        if let Some(scopes) = scopes_const {
            let paths: Vec<_> = scopes.iter().flat_map(|s| s.pretty_path(&db)).collect();
            println!("Cursor on 'NESTED_CONST': {:?}", paths);
            assert!(paths.iter().any(|p| p.contains("NESTED_CONST")), 
                    "Expected NESTED_CONST resolution, got: {:?}", paths);
        } else {
            panic!("No scopes found for cursor on NESTED_CONST");
        }
    }
}