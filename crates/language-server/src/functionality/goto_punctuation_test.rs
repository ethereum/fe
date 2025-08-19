#[cfg(test)]
mod punctuation_tests {
    use crate::functionality::goto::goto_definition_with_hir_synthesis;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use parser::TextSize;
    use url::Url;

    #[test]
    fn test_punctuation_doesnt_trigger_goto() {
        let mut db = DriverDataBase::default();

        let source = r#"
struct Foo {
    x: u32
}

fn main() {
    let a = Foo::new();  
    let b = 42;
}"#;

        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test 1: Position on "::" - should NOT trigger goto
        let colon_colon_pos = source.find("Foo::").unwrap() + 3; // Position on ::
        let cursor = TextSize::from(colon_colon_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(
            result.is_empty(),
            "Double colon :: should not trigger goto, but got {:?}",
            result
        );

        // Test 2: Position on "=" - should NOT trigger goto
        let equals_pos = source.find("let b =").unwrap() + 6; // Position on =
        let cursor2 = TextSize::from(equals_pos as u32);
        let result2 = goto_definition_with_hir_synthesis(&db, top_mod, cursor2, file).unwrap();
        assert!(
            result2.is_empty(),
            "Equals sign = should not trigger goto, but got {:?}",
            result2
        );

        // Test 3: Position on "Foo" identifier - SHOULD trigger goto
        let foo_pos = source.find("= Foo").unwrap() + 2; // Position on Foo after =
        let cursor3 = TextSize::from(foo_pos as u32);
        let result3 = goto_definition_with_hir_synthesis(&db, top_mod, cursor3, file).unwrap();
        assert!(!result3.is_empty(), "Foo identifier should trigger goto");

        // Test 4: Position on whitespace - should NOT trigger goto
        let space_pos = source.find("let ").unwrap() + 3; // Position on space after let
        let cursor4 = TextSize::from(space_pos as u32);
        let result4 = goto_definition_with_hir_synthesis(&db, top_mod, cursor4, file).unwrap();
        assert!(
            result4.is_empty(),
            "Whitespace should not trigger goto, but got {:?}",
            result4
        );
    }

    #[test]
    fn test_identifier_at_end_triggers_goto() {
        let mut db = DriverDataBase::default();

        let source = r#"
struct Bar {
    x: u32
}

fn main() {
    let b: Bar;
}"#;

        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test2.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // Test: Position at last character of "Bar" - SHOULD trigger goto
        let bar_last = source.find(": Bar").unwrap() + 4; // Position on 'r' of Bar
        let cursor = TextSize::from(bar_last as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(
            !result.is_empty(),
            "Last character of identifier should trigger goto"
        );
    }
}
