use common::InputDb;
use fe_driver2::{initialize_analysis_pass, DriverDataBase};
use hir::lower::module_tree;

#[test]
fn check_std_lib() {
    let mut driver = DriverDataBase::default();
    let std_ingot = library2::std_lib_input_ingot(&mut driver);
    driver.run_on_ingot(std_ingot);

    let diags = driver.format_diags();
    if !diags.is_empty() {
        panic!("{diags}")
    }
}
