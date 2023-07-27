#![cfg(feature = "solc-backend")]
use std::path::Path;

use clap::Args;
use colored::Colorize;
use fe_common::diagnostics::print_diagnostics;
use fe_common::utils::files::BuildFiles;
use fe_driver::CompiledTest;
use fe_test_runner::TestSink;

#[derive(Args)]
#[clap(about = "Execute tests in the current project")]
pub struct TestArgs {
    input_path: String,
    #[clap(long, takes_value(true))]
    optimize: Option<bool>,
}

pub fn test(args: TestArgs) {
    let path = &args.input_path;

    let test_sink = if Path::new(path).is_file() {
        test_single_file(&args)
    } else {
        test_ingot(&args)
    };

    println!("{test_sink}");
    if test_sink.failure_count() != 0 {
        std::process::exit(1)
    }
}

fn test_single_file(args: &TestArgs) -> TestSink {
    let input_path = &args.input_path;

    let mut db = fe_driver::Db::default();
    let content = match std::fs::read_to_string(input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{input_path}`. Error: {err}");
            std::process::exit(1)
        }
        Ok(content) => content,
    };

    match fe_driver::compile_single_file_tests(&mut db, input_path, &content, true) {
        Ok((name, tests)) => {
            let mut sink = TestSink::default();
            execute_tests(&name, &tests, &mut sink);
            sink
        }
        Err(error) => {
            eprintln!("Unable to compile {input_path}.");
            print_diagnostics(&db, &error.0);
            std::process::exit(1)
        }
    }
}

pub fn execute_tests(module_name: &str, tests: &[CompiledTest], sink: &mut TestSink) {
    if tests.len() == 1 {
        println!("executing 1 test in {module_name}:");
    } else {
        println!("executing {} tests in {}:", tests.len(), module_name);
    }

    for test in tests {
        print!("  {} ...", test.name);
        let test_passed = test.execute(sink);

        if test_passed {
            println!(" {}", "passed".green())
        } else {
            println!(" {}", "failed".red())
        }
    }
    println!();
}

fn test_ingot(args: &TestArgs) -> TestSink {
    let input_path = &args.input_path;
    let optimize = args.optimize.unwrap_or(true);

    if !Path::new(input_path).exists() {
        eprintln!("Input directory does not exist: `{input_path}`.");
        std::process::exit(1)
    }

    let build_files = match BuildFiles::load_fs(input_path) {
        Ok(files) => files,
        Err(err) => {
            eprintln!("Failed to load project files.\nError: {err}");
            std::process::exit(1)
        }
    };

    let mut db = fe_driver::Db::default();

    match fe_driver::compile_ingot_tests(&mut db, &build_files, optimize) {
        Ok(test_batches) => {
            let mut sink = TestSink::default();
            for (module_name, tests) in test_batches {
                execute_tests(&module_name, &tests, &mut sink);
            }
            sink
        }
        Err(error) => {
            eprintln!("Unable to compile {input_path}.");
            print_diagnostics(&db, &error.0);
            std::process::exit(1)
        }
    }
}
