#[cfg(test)]
mod manual_tests {
    use driver::DriverDataBase;
    use common::InputDb;
    use crate::functionality::goto::goto_definition_with_hir_synthesis;
    use hir::lower::map_file_to_mod;
    use url::Url;
    use parser::TextSize;

    #[test]
    fn test_goto_struct_type_annotation() {
        let mut db = DriverDataBase::default();
        
        let source = r#"
struct Foo {
    x: i32
}

fn main() {
    let f: Foo = Foo { x: 42 };
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Find the position of "Foo" in "let f: Foo"
        let foo_pos = source.find("let f: Foo").unwrap() + "let f: ".len();
        let cursor = TextSize::from(foo_pos as u32);
        
        // Test goto at the Foo type annotation
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Should find goto target for Foo type annotation");
        
        for target in &result {
            if let Some(path) = target.pretty_path(&db) {
                assert!(path.contains("Foo") || path.contains("local at"), "Should resolve to Foo struct or local");
            }
        }
    }

    #[test] 
    fn test_goto_struct_constructor() {
        let mut db = DriverDataBase::default();
        
        let source = r#"struct Bar {
    value: u32
}

fn test() {
    let b = Bar { value: 10 };
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test2.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Find "Bar" in the constructor (the second occurrence)
        let bar_str = "= Bar {";
        let bar_pos = source.find(bar_str).unwrap() + 2;  // Skip "= "
        let cursor = TextSize::from(bar_pos as u32);
        
        // Test goto at the Bar constructor
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Should find goto target for Bar constructor");
        
        for target in &result {
            if let Some(path) = target.pretty_path(&db) {
                assert!(path.contains("Bar") || path.contains("local at"), "Should resolve to Bar struct or local");
            }
        }
    }
}