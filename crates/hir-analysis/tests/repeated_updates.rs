mod test_db;

use fe_hir_analysis::{name_resolution::PathAnalysisPass, ty::FuncAnalysisPass};
use hir::{analysis_pass::AnalysisPassManager, lower::map_file_to_mod, LowerHirDb};
use test_db::HirAnalysisTestDb;

use salsa::Setter;

#[test]
fn test_updated() {
    let mut db = HirAnalysisTestDb::default();
    let file_name = "file.fe";
    let versions = vec![
        r#"fn foo() {}"#,
        r#"use bla
           fn foo() {}"#,
        r#"use bla::bla
           fn foo() {}"#,
        r#"use bla::bla::bla
           fn foo() {}"#,
        r#"use bla::bla::bla::bla
           fn foo() {}"#,
    ];

    let input = db.new_stand_alone(file_name, versions[0]);

    for version in versions {
        {
            let top_mod = map_file_to_mod(db.as_lower_hir_db(), input);
            let mut pass_manager = initialize_pass_manager(&db);
            let _ = pass_manager.run_on_module(top_mod);
        }

        {
            input.set_text(&mut db).to(version.into());
        }
    }
}

fn initialize_pass_manager(db: &HirAnalysisTestDb) -> AnalysisPassManager<'_> {
    let mut pass_manager = AnalysisPassManager::new();
    // pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    // pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(ImportAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(PathAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(TraitAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(ImplAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass::new(db)));
    // pass_manager.add_module_pass(Box::new(BodyAnalysisPass::new(db)));
    pass_manager
}
