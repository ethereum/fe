#[cfg(test)]
mod generic_bounds_tests {
    use driver::DriverDataBase;
    use common::InputDb;
    use crate::functionality::goto::goto_definition_with_hir_synthesis;
    use hir::lower::map_file_to_mod;
    use url::Url;
    use parser::TextSize;

    #[test]
    fn test_generic_type_bounds_resolution() {
        let mut db = DriverDataBase::default();
        
        let source = r#"
trait Inner {
    fn inner_method(self);
}

pub struct Wrapper<S: Inner> {
    pub inner: S,
}

impl<T: Inner> Wrapper<T> {
    fn new(inner: T) -> Self {
        Wrapper { inner }
    }
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Test 1: Inner in struct generic bounds should resolve to the trait
        let inner_in_struct = source.find("<S: Inner>").unwrap() + 4; // Position on 'I' of Inner
        let cursor = TextSize::from(inner_in_struct as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Inner in generic type bound should resolve to trait, but got no results");
        
        // Test 2: Inner in impl generic bounds should also resolve
        let inner_in_impl = source.find("<T: Inner>").unwrap() + 4; // Position on 'I' of Inner
        let cursor = TextSize::from(inner_in_impl as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Inner in impl generic bound should resolve to trait, but got no results");
    }
    
    #[test]
    fn test_multiple_bounds_resolution() {
        let mut db = DriverDataBase::default();
        
        let source = r#"
trait Foo {
    fn foo(self);
}

trait Bar {
    fn bar(self);
}

struct Complex<T: Foo + Bar> {
    value: T,
}"#;
        
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test2.fe").unwrap(),
            Some(source.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        
        // Test that both Foo and Bar resolve in multiple bounds
        let foo_pos = source.find("T: Foo").unwrap() + 3; // Position on 'F' of Foo
        let cursor = TextSize::from(foo_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Foo in multiple bounds should resolve");
        
        let bar_pos = source.find("+ Bar").unwrap() + 2; // Position on 'B' of Bar
        let cursor = TextSize::from(bar_pos as u32);
        let result = goto_definition_with_hir_synthesis(&db, top_mod, cursor, file).unwrap();
        assert!(!result.is_empty(), "Bar in multiple bounds should resolve");
    }
}