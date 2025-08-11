use driver::DriverDataBase;
use common::InputDb;
use url::Url;

fn main() {
    let mut db = DriverDataBase::default();
    
    let source = r#"struct Foo {}

fn test() {
    let x: Foo
}"#;
    
    // Add file to workspace
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///test.fe").unwrap(),
        Some(source.to_string()),
    );
    
    // Map to module
    let top_mod = fe_hir::lower::map_file_to_mod(&db, file);
    
    // Test cursor at "Foo" in type annotation (position around 28)
    // "struct Foo {}\n\nfn test() {\n    let x: Foo"
    //                                          ^^^ this is what we want
    let cursor = parser::TextSize::from(40); // "Foo" in the type annotation
    
    println!("Testing goto at cursor position {}", cursor);
    
    // Call the goto function
    match fe_language_server::functionality::goto::goto_definition_with_hir_synthesis(&db, top_mod, cursor) {
        Ok(scopes) => {
            println!("Found {} scopes", scopes.len());
            for scope in scopes {
                if let Some(path) = scope.pretty_path(&db) {
                    println!("  Scope: {}", path);
                }
            }
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}