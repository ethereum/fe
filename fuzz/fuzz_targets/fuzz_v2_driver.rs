#![no_main]
extern crate fe_driver2;
extern crate libfuzzer_sys;

use libfuzzer_sys::fuzz_target;
use fe_driver2::DriverDataBase;

fuzz_target!(|data: &[u8]| {
    // Convert the fuzzing engine input to a string
    let input = match String::from_utf8(data.to_vec()) {
        Ok(s) => s,
        Err(_) => return,
    };

    let mut db = DriverDataBase::default();
    let top_mod = db.top_mod_from_file(
        std::path::Path::new("dummy.fe"),
        &input
    );
    db.run_on_top_mod(top_mod);
    db.emit_diags();
});
