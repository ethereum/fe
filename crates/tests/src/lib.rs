#![cfg(feature = "solc-backend")]
#![allow(dead_code)]
use std::path::Path;

use dir_test::{dir_test, Fixture};
use fe_common::diagnostics::print_diagnostics;
use fe_common::utils::files::load_fe_files_from_dir;
use fe_test_runner::TestSink;

use include_dir::{include_dir, Dir};

const FIXTURES: Dir = include_dir!("$CARGO_MANIFEST_DIR/fixtures");

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/fixtures/ingots/", glob: "**/main.fe")]
fn ingot_test_run(fixture: Fixture<&str>) {
    let input_path = fixture.path().trim_end_matches("/main.fe");
    let optimize = true;

    if !Path::new(input_path).exists() {
        panic!("Input directory does not exist: `{input_path}`.");
    }

    let content = match load_fe_files_from_dir(input_path) {
        Ok(files) if files.is_empty() => {
            panic!("Input directory is not an ingot: `{input_path}`");
        }
        Ok(files) => files,
        Err(err) => {
            panic!("Failed to load project files. Error: {err}");
        }
    };

    let mut db = fe_driver::Db::default();
    match fe_driver::compile_ingot_tests(&mut db, input_path, &content, optimize) {
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

/// Returns `(file_path, file_content)`
pub fn fixture_dir_files(path: &str) -> Vec<(&'static str, &'static str)> {
    let dir = FIXTURES
        .get_dir(path)
        .unwrap_or_else(|| panic!("no fixture dir named \"{path}\""));

    fe_library::static_dir_files(dir)
}
