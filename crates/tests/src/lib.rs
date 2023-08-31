#![cfg(feature = "solc-backend")]
#![allow(dead_code)]
use std::path::Path;

use dir_test::{dir_test, Fixture};
use fe_common::diagnostics::print_diagnostics;
use fe_common::utils::files::BuildFiles;
use fe_test_runner::TestSink;

#[dir_test(dir: "$CARGO_MANIFEST_DIR/fixtures/files", glob: "*.fe")]
fn single_file_test_run(fixture: Fixture<&str>) {
    let mut db = fe_driver::Db::default();
    let tests = match fe_driver::compile_single_file_tests(
        &mut db,
        fixture.path(),
        fixture.content(),
        true,
    ) {
        Ok((_, tests)) => tests,
        Err(error) => {
            eprintln!("Unable to compile {}.", fixture.path());
            print_diagnostics(&db, &error.0);
            panic!("failed to compile tests")
        }
    };

    let mut test_sink = TestSink::default();

    for test in tests {
        test.execute(&mut test_sink);
    }

    if test_sink.failure_count() != 0 {
        panic!("{}", test_sink)
    }
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/fixtures/ingots/", glob: "**/fe.toml")]
fn ingot_test_run(fixture: Fixture<&str>) {
    let input_path = fixture.path().trim_end_matches("/fe.toml");
    let optimize = true;

    if !Path::new(input_path).exists() {
        panic!("Input directory does not exist: `{input_path}`.");
    }

    let build_files =
        BuildFiles::load_fs(input_path).expect("failed to load build files from file system");

    let mut db = fe_driver::Db::default();
    match fe_driver::compile_ingot_tests(&mut db, &build_files, optimize) {
        Ok(test_batches) => {
            let mut sink = TestSink::default();
            for (_, tests) in test_batches {
                for test in tests {
                    test.execute(&mut sink);
                }
                if sink.failure_count() != 0 {
                    panic!("{}", sink)
                }
            }
        }
        Err(error) => {
            print_diagnostics(&db, &error.0);
            panic!("Unable to compile {input_path}.");
        }
    }
}
