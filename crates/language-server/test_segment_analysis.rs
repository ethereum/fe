use common::InputDb;
use driver::DriverDataBase;
use hir::{
    hir_def::TopLevelMod,
    lower::map_file_to_mod,
    SpannedHirDb,
};
use url::Url;

fn main() {
    let mut db = DriverDataBase::default();
    
    let source = r#"
mod nested {
    pub const NESTED_CONST: u32 = 100
}

fn test_nested() {
    let a = nested::NESTED_CONST
}"#;
    
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path("/test.fe").unwrap(),
        Some(source.to_string()),
    );
    let top_mod = map_file_to_mod(&db, file);
    
    // Collect resolvable positions
    let positions = hir_analysis::tooling_api::collect_resolvable_positions(&db, top_mod);
    
    println!("Found {} resolvable positions", positions.len());
    
    for (idx, position) in positions.iter().enumerate() {
        match position {
            hir_analysis::tooling_api::ResolvablePosition::Path(path, _scope, lazy_span) => {
                let path_str = path.pretty_print(&db);
                let segment_count = path.segment_index(&db) + 1;
                
                println!("\n[{}] Path: {} (segments: {})", idx, path_str, segment_count);
                
                // Try to resolve the full path span
                if let Some(full_span) = lazy_span.resolve(&db) {
                    println!("  Full path span: {:?}", full_span.range);
                } else {
                    println!("  Full path span: NOT RESOLVED");
                }
                
                // Try to resolve individual segments
                for seg_idx in 0..segment_count {
                    if let Some(seg_span) = lazy_span.clone().segment(seg_idx).resolve(&db) {
                        println!("  Segment {} span: {:?}", seg_idx, seg_span.range);
                    } else {
                        println!("  Segment {} span: NOT RESOLVED", seg_idx);
                    }
                }
            }
            _ => {}
        }
    }
}