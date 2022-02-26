//! The `fe` command-line interface.

use std::fs;
use std::io::{Error, Write};
use std::path::Path;

use clap::{arg_enum, values_t, App, Arg};

use fe_common::diagnostics::print_diagnostics;
use fe_common::files::SourceFileId;
use fe_common::panic::install_panic_hook;
use fe_driver::{CompiledModule, Db};
use walkdir::WalkDir;

const DEFAULT_OUTPUT_DIR_NAME: &str = "output";
const VERSION: &str = env!("CARGO_PKG_VERSION");

arg_enum! {
    #[derive(PartialEq, Debug)]
    pub enum CompilationTarget {
        Abi,
        Ast,
        LoweredAst,
        Bytecode,
        Tokens,
        Yul,
    }
}

pub fn main() {
    install_panic_hook();

    let matches = App::new("Fe")
        .version(VERSION)
        .about("Compiler for the Fe language")
        .arg(
            Arg::with_name("input")
                .help("The input source file to use e.g erc20.fe")
                .index(1)
                .required(true),
        )
        .arg(
            Arg::with_name("output-dir")
                .short("o")
                .long("output-dir")
                .help("The directory to store the compiler output e.g /tmp/output")
                .takes_value(true)
                .default_value(DEFAULT_OUTPUT_DIR_NAME),
        )
        .arg(
            Arg::with_name("emit")
                .short("e")
                .long("emit")
                .help("Comma separated compile targets e.g. -e=bytecode,yul")
                .possible_values(&["abi", "bytecode", "ast", "tokens", "yul", "loweredAst"])
                .default_value("abi,bytecode")
                .use_delimiter(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("overwrite")
                .long("overwrite")
                .help("Overwrite contents of output directory`"),
        )
        .arg(
            Arg::with_name("optimize")
                .long("optimize")
                .help("Enables the Yul optimizer`")
                .possible_values(&["true", "false"])
                .default_value("true")
                .use_delimiter(false)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("mir")
                .long("mir")
                .help("dump mir dot file")
                .takes_value(false),
        )
        .get_matches();

    let input_path = matches.value_of("input").unwrap();
    let output_dir = matches.value_of("output-dir").unwrap();
    let overwrite = matches.is_present("overwrite");
    let optimize = matches.value_of("optimize") == Some("true");
    let targets =
        values_t!(matches.values_of("emit"), CompilationTarget).unwrap_or_else(|e| e.exit());
    let with_bytecode = targets.contains(&CompilationTarget::Bytecode);

    if matches.is_present("mir") {
        return mir_dump(input_path);
    }
    #[cfg(not(feature = "solc-backend"))]
    if with_bytecode {
        eprintln!("Warning: bytecode output requires 'solc-backend' feature. Try `cargo build --release --features solc-backend`. Skipping.");
    }

    let mut db = Db::default();

    let (content, compiled_module) = if Path::new(input_path).is_file() {
        let content = match std::fs::read_to_string(input_path) {
            Err(err) => {
                eprintln!("Failed to load file: `{}`. Error: {}", input_path, err);
                std::process::exit(1)
            }
            Ok(content) => content,
        };

        let compiled_module = match fe_driver::compile_single_file(
            &mut db,
            input_path,
            &content,
            with_bytecode,
            optimize,
        ) {
            Ok(module) => module,
            Err(error) => {
                eprintln!("Unable to compile {}.", input_path);
                print_diagnostics(&db, &error.0);
                std::process::exit(1)
            }
        };
        (content, compiled_module)
    } else {
        if !Path::new(input_path).exists() {
            eprintln!("Input directory does not exist: `{}`.", input_path);
            std::process::exit(1)
        }

        let files = match load_files_from_dir(input_path) {
            Ok(files) if files.is_empty() => {
                eprintln!("Input directory is not an ingot: `{}`", input_path);
                std::process::exit(1)
            }
            Ok(files) => files,
            Err(err) => {
                eprintln!("Failed to load project files. Error: {}", err);
                std::process::exit(1)
            }
        };

        let compiled_module = match fe_driver::compile_ingot(
            &mut db,
            "main", // TODO: real ingot name
            &files,
            with_bytecode,
            optimize,
        ) {
            Ok(module) => module,
            Err(error) => {
                eprintln!("Unable to compile {}.", input_path);
                print_diagnostics(&db, &error.0);
                std::process::exit(1)
            }
        };

        // no file content for ingots
        ("".to_string(), compiled_module)
    };

    match write_compiled_module(compiled_module, &content, &targets, output_dir, overwrite) {
        Ok(_) => println!("Compiled {}. Outputs in `{}`", input_path, output_dir),
        Err(err) => {
            eprintln!(
                "Failed to write output to directory: `{}`. Error: {}",
                output_dir, err
            );
            std::process::exit(1)
        }
    }
}

fn load_files_from_dir(dir_path: &str) -> Result<Vec<(String, String)>, std::io::Error> {
    let entries = WalkDir::new(dir_path);
    let mut files = vec![];
    for entry in entries.into_iter() {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().and_then(std::ffi::OsStr::to_str) == Some("fe") {
            let content = std::fs::read_to_string(path)?;
            files.push((path.to_string_lossy().to_string(), content));
        }
    }
    Ok(files)
}

fn write_compiled_module(
    mut module: CompiledModule,
    file_content: &str,
    targets: &[CompilationTarget],
    output_dir: &str,
    overwrite: bool,
) -> Result<(), String> {
    let output_dir = Path::new(output_dir);
    if output_dir.is_file() {
        return Err(format!(
            "A file exists at path `{}`, the location of the output directory. Refusing to overwrite.",
            output_dir.display()
        ));
    }

    if !overwrite {
        verify_nonexistent_or_empty(output_dir)?;
    }

    fs::create_dir_all(output_dir).map_err(ioerr_to_string)?;

    if targets.contains(&CompilationTarget::Ast) {
        write_output(&output_dir.join("module.ast"), &module.src_ast)?;
    }

    if targets.contains(&CompilationTarget::LoweredAst) {
        write_output(&output_dir.join("lowered_module.ast"), &module.lowered_ast)?;
    }

    if targets.contains(&CompilationTarget::Tokens) {
        let tokens = {
            let lexer = fe_parser::lexer::Lexer::new(SourceFileId::dummy_file(), file_content);
            lexer.collect::<Vec<_>>()
        };
        write_output(&output_dir.join("module.tokens"), &format!("{:#?}", tokens))?;
    }

    for (name, contract) in module.contracts.drain(0..) {
        let contract_output_dir = output_dir.join(&name);
        fs::create_dir_all(&contract_output_dir).map_err(ioerr_to_string)?;

        if targets.contains(&CompilationTarget::Abi) {
            let file_name = format!("{}_abi.json", &name);
            write_output(&contract_output_dir.join(file_name), &contract.json_abi)?;
        }

        if targets.contains(&CompilationTarget::Yul) {
            let file_name = format!("{}_ir.yul", &name);
            write_output(&contract_output_dir.join(file_name), &contract.yul)?;
        }

        #[cfg(feature = "solc-backend")]
        if targets.contains(&CompilationTarget::Bytecode) {
            let file_name = format!("{}.bin", &name);
            write_output(&contract_output_dir.join(file_name), &contract.bytecode)?;
        }
    }

    Ok(())
}

fn write_output(path: &Path, content: &str) -> Result<(), String> {
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .map_err(ioerr_to_string)?;
    file.write_all(content.as_bytes())
        .map_err(ioerr_to_string)?;
    Ok(())
}

fn ioerr_to_string(error: Error) -> String {
    format!("{}", error)
}

fn verify_nonexistent_or_empty(dir: &Path) -> Result<(), String> {
    if !dir.exists() || dir.read_dir().map_err(ioerr_to_string)?.next().is_none() {
        Ok(())
    } else {
        Err(format!(
            "Directory '{}' is not empty. Use --overwrite to overwrite.",
            dir.display()
        ))
    }
}

fn mir_dump(input_path: &str) {
    let mut db = fe_driver::NewDb::default();
    if Path::new(input_path).is_file() {
        let content = match std::fs::read_to_string(input_path) {
            Err(err) => {
                eprintln!("Failed to load file: `{}`. Error: {}", input_path, err);
                std::process::exit(1)
            }
            Ok(content) => content,
        };

        match fe_driver::dump_mir_single_file(&mut db, input_path, &content) {
            Ok(text) => println!("{}", text),
            Err(err) => {
                eprintln!("Unable to dump mir `{}", input_path);
                print_diagnostics(&db, &err.0);
                std::process::exit(1)
            }
        }
    } else {
        eprintln!("mir doesn't support ingot yet");
        std::process::exit(1)
    }
}
