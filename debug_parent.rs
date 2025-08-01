use fe_compiler_test_utils::*;
use fe_driver::db::Upcast;
use fe_hir::{HirDb, hir_def::{ItemKind, ScopeId}};

fn main() {
    let mut db = fe_db::TestDb::default();
    let content = std::fs::read_to_string("debug_parent.fe").unwrap();
    let file = fe_driver::compile_single_file_lib_std(&mut db, "test.fe", &content).unwrap();
    
    // Find the scope graph for the module
    let top_mod = file.top_mod;
    let scope_graph = top_mod.scope_graph(db.upcast());
    
    // Walk through all items
    for item in scope_graph.items_dfs(db.upcast()) {
        if let ItemKind::Func(func) = item {
            let name = func.name(db.upcast());
            if let Some(name) = name.to_opt() {
                let name_str = name.data(db.upcast());
                println!("Found function: {}", name_str);
                
                // Get the function's scope and parent
                let func_scope = ScopeId::from_item(item);
                println!("  Function scope: {:?}", func_scope);
                
                // Try to get parent
                if let Some(parent) = func_scope.parent(db.upcast()) {
                    println!("  Parent scope: {:?}", parent);
                    
                    // Try pretty_path
                    if let Some(pretty) = func_scope.pretty_path(db.upcast()) {
                        println!("  Pretty path: {}", pretty);
                    } else {
                        println!("  Pretty path: None");
                        
                        // Debug parent's pretty path
                        if let Some(parent_pretty) = parent.pretty_path(db.upcast()) {
                            println!("  Parent pretty path: {}", parent_pretty);
                        } else {
                            println!("  Parent pretty path: None");
                        }
                    }
                } else {
                    println!("  No parent!");
                }
            }
        }
    }
}