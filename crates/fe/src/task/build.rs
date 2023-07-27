use std::fs;
use std::io::{Error, Write};
use std::path::Path;

use clap::{ArgEnum, Args};
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::SourceFileId;
use fe_common::utils::files::{BuildFiles, ProjectMode};
use fe_driver::CompiledModule;

const DEFAULT_OUTPUT_DIR_NAME: &str = "output";

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ArgEnum, Debug)]
enum Emit {
    Abi,
    Ast,
    LoweredAst,
    Bytecode,
    Tokens,
    Yul,
}

#[derive(Args)]
#[clap(about = "Build the current project")]
pub struct BuildArgs {
    input_path: String,
    #[clap(short, long, default_value = DEFAULT_OUTPUT_DIR_NAME)]
    output_dir: String,
    #[clap(
        arg_enum,
        use_value_delimiter = true,
        long,
        short,
        default_value = "abi,bytecode"
    )]
    emit: Vec<Emit>,
    #[clap(long)]
    mir: bool,
    #[clap(long)]
    overwrite: bool,
    #[clap(long, takes_value(true))]
    optimize: Option<bool>,
}

fn build_single_file(compile_arg: &BuildArgs) -> (String, CompiledModule) {
    let emit = &compile_arg.emit;
    let with_bytecode = emit.contains(&Emit::Bytecode);
    let input_path = &compile_arg.input_path;
    let optimize = compile_arg.optimize.unwrap_or(true);

    let mut db = fe_driver::Db::default();
    let content = match std::fs::read_to_string(input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{input_path}`. Error: {err}");
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
            eprintln!("Unable to compile {input_path}.");
            print_diagnostics(&db, &error.0);
            std::process::exit(1)
        }
    };
    (content, compiled_module)
}

fn build_ingot(compile_arg: &BuildArgs) -> (String, CompiledModule) {
    let emit = &compile_arg.emit;
    let with_bytecode = emit.contains(&Emit::Bytecode);
    let input_path = &compile_arg.input_path;
    let optimize = compile_arg.optimize.unwrap_or(true);

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

    if build_files.root_project_mode() == ProjectMode::Lib {
        eprintln!("Unable to compile {input_path}. No build targets in library mode.");
        eprintln!("Consider replacing `src/lib.fe` with `src/main.fe`.");
        std::process::exit(1)
    }

    let mut db = fe_driver::Db::default();
    let compiled_module =
        match fe_driver::compile_ingot(&mut db, &build_files, with_bytecode, optimize) {
            Ok(module) => module,
            Err(error) => {
                eprintln!("Unable to compile {input_path}.");
                print_diagnostics(&db, &error.0);
                std::process::exit(1)
            }
        };

    // no file content for ingots
    ("".to_string(), compiled_module)
}

pub fn build(compile_arg: BuildArgs) {
    let emit = &compile_arg.emit;

    let input_path = &compile_arg.input_path;

    if compile_arg.mir {
        return mir_dump(input_path);
    }

    let _with_bytecode = emit.contains(&Emit::Bytecode);
    #[cfg(not(feature = "solc-backend"))]
    if _with_bytecode {
        eprintln!("Warning: bytecode output requires 'solc-backend' feature. Try `cargo build --release --features solc-backend`. Skipping.");
    }

    let (content, compiled_module) = if Path::new(input_path).is_file() {
        build_single_file(&compile_arg)
    } else {
        build_ingot(&compile_arg)
    };

    let output_dir = &compile_arg.output_dir;
    let overwrite = compile_arg.overwrite;
    match write_compiled_module(compiled_module, &content, emit, output_dir, overwrite) {
        Ok(_) => eprintln!("Compiled {input_path}. Outputs in `{output_dir}`"),
        Err(err) => {
            eprintln!("Failed to write output to directory: `{output_dir}`. Error: {err}");
            std::process::exit(1)
        }
    }
}

fn write_compiled_module(
    mut module: CompiledModule,
    file_content: &str,
    targets: &[Emit],
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

    if targets.contains(&Emit::Ast) {
        write_output(&output_dir.join("module.ast"), &module.src_ast)?;
    }

    if targets.contains(&Emit::LoweredAst) {
        write_output(&output_dir.join("lowered_module.ast"), &module.lowered_ast)?;
    }

    if targets.contains(&Emit::Tokens) {
        let tokens = {
            let lexer = fe_parser::lexer::Lexer::new(SourceFileId::dummy_file(), file_content);
            lexer.collect::<Vec<_>>()
        };
        write_output(&output_dir.join("module.tokens"), &format!("{tokens:#?}"))?;
    }

    for (name, contract) in module.contracts.drain(0..) {
        let contract_output_dir = output_dir.join(&name);
        fs::create_dir_all(&contract_output_dir).map_err(ioerr_to_string)?;

        if targets.contains(&Emit::Abi) {
            let file_name = format!("{}_abi.json", &name);
            write_output(&contract_output_dir.join(file_name), &contract.json_abi)?;
        }

        if targets.contains(&Emit::Yul) {
            let file_name = format!("{}_ir.yul", &name);
            write_output(&contract_output_dir.join(file_name), &contract.yul)?;
        }

        #[cfg(feature = "solc-backend")]
        if targets.contains(&Emit::Bytecode) {
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
    format!("{error}")
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
    let mut db = fe_driver::Db::default();
    if Path::new(input_path).is_file() {
        let content = match std::fs::read_to_string(input_path) {
            Err(err) => {
                eprintln!("Failed to load file: `{input_path}`. Error: {err}");
                std::process::exit(1)
            }
            Ok(content) => content,
        };

        match fe_driver::dump_mir_single_file(&mut db, input_path, &content) {
            Ok(text) => println!("{text}"),
            Err(err) => {
                eprintln!("Unable to dump mir `{input_path}");
                print_diagnostics(&db, &err.0);
                std::process::exit(1)
            }
        }
    } else {
        eprintln!("dumping mir for ingot is not supported yet");
        std::process::exit(1)
    }
}
