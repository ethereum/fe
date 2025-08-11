use fe_language_server::functionality::goto::*;
use fe_driver::DriverDataBase;
use fe_hir::lower::map_file_to_mod;
use fe_common::InputDb;
use url::Url;

fn main() {
    let mut db = DriverDataBase::default();
    let content = std::fs::read_to_string("test_debug.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(std::env::current_dir().unwrap().join("test_debug.fe")).unwrap(),
        Some(content.clone()),
    );
    let top_mod = map_file_to_mod(&db, file);
    
    // Try to get goto for "Point" at position (6, 11) - the type annotation
    let cursor = fe_language_server::util::to_offset_from_position(
        async_lsp::lsp_types::Position::new(6, 11),
        &content
    );
    
    println!("Testing cursor at offset: {}", cursor);
    
    match goto_definition_with_hir_synthesis(&db, top_mod, cursor) {
        Ok(scopes) => {
            println!("Found {} scopes", scopes.len());
            for scope in scopes {
                println!("  - {:?}", scope.pretty_path(&db));
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}