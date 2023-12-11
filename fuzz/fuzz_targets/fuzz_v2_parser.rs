#![no_main]
extern crate libfuzzer_sys;
extern crate fe_parser2;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Convert the fuzzing engine input to a string
    let input = match String::from_utf8(data.to_vec()) {
        Ok(s) => s,
        Err(_) => "".to_owned(),
    };
    fe_parser2::parse_source_file(&input);
});
