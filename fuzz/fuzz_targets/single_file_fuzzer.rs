#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate fe_common;
extern crate fe_driver;

use fe_common::diagnostics;

fuzz_target!(|data: &[u8]| {
    // Convert the fuzzing engine input to a string
    let input = String::from_utf8_lossy(data);

    // Create a mutable database
    let mut db = fe_driver::Db::default();

    // Call the `compile_single_file` API with the input
    let _ = match fe_driver::compile_single_file(
        &mut db, "dummy", &input, /*with_bytecode=*/ true,
        /*with_runtime_bytecode=*/ true, /*optimize=*/ true,
    ) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("Unable to compile input.");
            diagnostics::print_diagnostics(&db, &error.0);
        }
    };
});
