#[cfg(test)]
mod comprehensive_punctuation_tests {
    use driver::DriverDataBase;
    use common::InputDb;
    use crate::functionality::goto::goto_definition_with_hir_synthesis;
    use hir::lower::map_file_to_mod;
    use url::Url;
    use parser::TextSize;

    #[test]
    fn test_comprehensive_punctuation_blocking() {
        let mut db = DriverDataBase::default();
        
        let source = r#"
struct Foo {
    bar: u32
}

impl Foo {
    fn new() -> Self {
        Foo { bar: 0 }
    }
}

fn main() {
    let x = Foo::new();
    let y: Foo = Foo { bar: 42 };
    let z = x.bar;
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Test 1: :: in Foo::new() should NOT trigger goto
        let double_colon_pos = source.find("Foo::new").unwrap() + 3; // Position on ::
        let cursor = TextSize::from(double_colon_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), ":: in Foo::new should not trigger goto, but got {:?}", result);
        
        // Test 2: ( after new should NOT trigger goto
        let paren_pos = source.find("new()").unwrap() + 3; // Position on (
        let cursor = TextSize::from(paren_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), "( should not trigger goto, but got {:?}", result);
        
        // Test 3: { in struct literal should NOT trigger goto
        let brace_pos = source.find("Foo { bar").unwrap() + 4; // Position on {
        let cursor = TextSize::from(brace_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), "{{ should not trigger goto, but got {:?}", result);
        
        // Test 4: : in field assignment should NOT trigger goto
        let colon_pos = source.find("bar: 42").unwrap() + 3; // Position on :
        let cursor = TextSize::from(colon_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), ": in field assignment should not trigger goto, but got {:?}", result);
        
        // Test 5: . in field access should NOT trigger goto
        let dot_pos = source.find("x.bar").unwrap() + 1; // Position on .
        let cursor = TextSize::from(dot_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), ". in field access should not trigger goto, but got {:?}", result);
        
        // Test 6: -> in return type should NOT trigger goto
        let arrow_pos = source.find("-> Self").unwrap() + 1; // Position on >
        let cursor = TextSize::from(arrow_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), "-> arrow should not trigger goto, but got {:?}", result);
        
        // Test 7: = in assignment should NOT trigger goto
        let equals_pos = source.find("let x =").unwrap() + 6; // Position on =
        let cursor = TextSize::from(equals_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(result.is_empty(), "= should not trigger goto, but got {:?}", result);
    }
    
    #[test]
    fn test_identifiers_still_work() {
        let mut db = DriverDataBase::default();
        
        let source = r#"
struct Bar {
    x: u32
}

fn main() {
    let b = Bar { x: 1 };
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test2.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Test that identifiers still trigger goto
        
        // Position on 'B' of Bar in constructor
        let bar_pos = source.find("= Bar").unwrap() + 2; // Position on B
        let cursor = TextSize::from(bar_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Bar identifier should trigger goto");
        
        // Position on 'a' of Bar (middle of identifier)
        let bar_middle = source.find("= Bar").unwrap() + 3; // Position on a
        let cursor = TextSize::from(bar_middle as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Middle of Bar identifier should trigger goto");
        
        // Position on 'r' of Bar (end of identifier)
        let bar_end = source.find("= Bar").unwrap() + 4; // Position on r
        let cursor = TextSize::from(bar_end as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "End of Bar identifier should trigger goto");
    }
}