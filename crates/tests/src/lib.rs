#![cfg(feature = "solc-backend")]
#![allow(dead_code)]
use dir_test::{dir_test, Fixture};
use fe_test_runner::TestSink;

#[dir_test(dir: "$CARGO_MANIFEST_DIR/fixtures/files", glob: "*.fe")]
fn single_file_test_run(fixture: Fixture<&str>) {
    let mut db = fe_driver::Db::default();
    let tests =
        fe_driver::compile_single_file_tests(&mut db, fixture.path(), fixture.content(), true)
            .expect("failed to compile tests")
            .1;

    let mut test_sink = TestSink::default();

    for test in tests {
        test.execute(&mut test_sink);
    }

    if test_sink.failure_count() != 0 {
        panic!("{}", test_sink)
    }
}

// TODO: implement ingot test runner
