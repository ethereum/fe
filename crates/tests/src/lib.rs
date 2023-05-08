#![cfg(feature = "solc-backend")]
#![allow(dead_code)]
use dir_test::{dir_test, Fixture};
use fe_common::diagnostics::print_diagnostics;
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
