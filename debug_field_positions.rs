use driver::Db;
use hir_analysis::tooling_api::{collect_resolvable_positions, ResolvablePosition};
use std::sync::Arc;

fn main() {
    let db = Db::from_single_file_with_name("debug_field_receiver.fe", 
        r#"pub fn test() {
    let self = Container { value: 42 };
    let x = self.value; // Here, 'self' should be resolvable
}

struct Container {
    pub value: u32,
}"#,
    ).unwrap();

    let top_mod = db.top_mod();
    let positions = collect_resolvable_positions(&db, top_mod);
    
    println!("Found {} positions:", positions.len());
    for (i, pos) in positions.iter().enumerate() {
        match pos {
            ResolvablePosition::LocalBinding(ident, _scope, _span) => {
                let ident_name = ident.data(&db);
                println!("  {}: LocalBinding({})", i, ident_name);
            }
            ResolvablePosition::FieldAccess(expr, ident, _scope, _span) => {
                let ident_name = ident.data(&db);
                println!("  {}: FieldAccess(expr_id={:?}, field={})", i, expr, ident_name);
            }
            ResolvablePosition::Path(path, _scope, _span) => {
                if let Some(ident) = path.ident(&db).to_opt() {
                    let ident_name = ident.data(&db);
                    println!("  {}: Path({})", i, ident_name);
                }
            }
            _ => {
                println!("  {}: {:?}", i, pos);
            }
        }
    }
}