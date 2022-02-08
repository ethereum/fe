use clap::ArgEnum;
use fe_driver::config;
use std::path::PathBuf;

#[derive(ArgEnum, PartialEq, Debug, Copy, Clone)]
pub enum CompilationTarget {
    Abi,
    Ast,
    LoweredAst,
    Bytecode,
    Tokens,
    Yul,
}

#[derive(clap::Args, Debug)]
pub struct BuildOpts {
    /// Comma separated list of compilation targets e.g. -e=bytecode,yul
    #[clap(arg_enum, short, long, default_value = "abi,bytecode")]
    emit: Option<Vec<CompilationTarget>>, // XXX enum

    /// Build the project or standalone Fe source file at the given path.
    #[clap(short, long)]
    path: Option<PathBuf>,

    /// Write the compiled output files to the specified directory.
    /// Defaults to the `target` directory inside the project directory.
    #[clap(short, long, default_value = "target")]
    output: PathBuf,

    // XXX default to true if the default target dir is used for a project build
    #[clap(long)]
    overwrite: bool,

    #[clap(long)]
    optimize: bool,
}

// XXX anyhow error
pub fn build(opts: BuildOpts) -> anyhow::Result<()> {
    if let Some(path) = opts.path {
        match path.metadata() {
            Ok(meta) => {
                if meta.is_dir() {
                    let manifest_path = path.join(config::MANIFEST_FILE_NAME);
                    if manifest_path.exists() {
                        todo!()
                        // build_project_with_manifest(manifest_path)
                    } else {
                        todo!("missing manifest") // XXX
                    }
                // } else if path.file_name() == Some(config::MANIFEST_FILE_NAME) {
                //     todo!()
                // build_project_with_manifest(path)
                } else {
                    todo!()
                    // build_single_file(path)
                }
            }
            Err(_err) => todo!(), // XXX handle bad path
        }
    } else {
        // find fe.toml

        // let wd = std::env::current_dir();
        todo!()
        // let mut files = build_ingot_filestore_for_dir(input_path);
        // let ingot_files = files.all_files();
        // let deps = files.add_included_libraries();

        // if !Path::new(input_path).exists() {
        //     eprintln!("Input directory does not exist: `{}`.", input_path);
        //     std::process::exit(1)
        // }

        // let compiled_module = match fe_driver::compile_ingot(
        //     input_path,
        //     &files,
        //     &ingot_files,
        //     &deps,
        //     with_bytecode,
        //     optimize,
        // ) {
        //     Ok(module) => module,
        //     Err(error) => {
        //         eprintln!("Unable to compile {}.", input_path);
        //         print_diagnostics(&error.0, &files);
        //         std::process::exit(1)
        //     }
        // };

        // // no file content for ingots
        // ("".to_string(), compiled_module)
    }

    // let with_bytecode = emit.contains(&CompilationTarget::Bytecode);
    // #[cfg(not(feature = "solc-backend"))]
    // if with_bytecode {
    //     eprintln!("Warning: bytecode output requires 'solc-backend' feature. Try `cargo build --release --features solc-backend`. Skipping.");
    // }
}

// fn build_single_file(
//     path: &Path,
//     emit: &[CompilationTarget],
//     output: &Path,
//     overwrite: Overwrite,
//     optimize: Optimize,
// ) {
// }

// fn load_source_dir(path: &str) -> FileStore {
//     let path = Path::new(path);
//     let walker = WalkDir::new(path);
//     let mut files = FileStore::new();

//     for entry in walker {
//         let entry = entry.unwrap();
//         let file_path = &entry.path().to_string_lossy().to_string();

//         if entry.path().extension() == Some(OsStr::new("fe")) {
//             let file = files.load_file(file_path);
//             if let Err(err) = file {
//                 eprintln!("Failed to load file: `{}`. Error: {}", &file_path, err);
//                 std::process::exit(1)
//             }
//         }
//     }

//     files
// }

// fn build_ingot_filestore_for_dir(path: &str) -> FileStore {
//     let path = Path::new(path);
//     let walker = WalkDir::new(path);
//     let mut files = FileStore::new();

//     for entry in walker {
//         let entry = entry.unwrap();
//         let file_path = &entry.path().to_string_lossy().to_string();

//         if entry.path().extension() == Some(OsStr::new("fe")) {
//             let file = files.load_file(file_path);
//             if let Err(err) = file {
//                 eprintln!("Failed to load file: `{}`. Error: {}", &file_path, err);
//                 std::process::exit(1)
//             }
//         }
//     }

//     files
// }

// fn write_compiled_module(
//     mut module: CompiledModule,
//     file_content: &str,
//     targets: &[CompilationTarget],
//     output_dir: &str,
//     overwrite: bool,
// ) -> Result<(), String> {
//     let output_dir = Path::new(output_dir);
//     if output_dir.is_file() {
//         return Err(format!(
//             "A file exists at path `{}`, the location of the output directory. Refusing to overwrite.",
//             output_dir.display()
//         ));
//     }

//     if !overwrite {
//         verify_nonexistent_or_empty(output_dir)?;
//     }

//     fs::create_dir_all(output_dir).map_err(ioerr_to_string)?;

//     if targets.contains(&CompilationTarget::Ast) {
//         write_output(&output_dir.join("module.ast"), &module.src_ast)?;
//     }

//     if targets.contains(&CompilationTarget::LoweredAst) {
//         write_output(&output_dir.join("lowered_module.ast"), &module.lowered_ast)?;
//     }

//     if targets.contains(&CompilationTarget::Tokens) {
//         // XXX
//         // let tokens = {
//         //     let lexer = fe_parser::lexer::Lexer::new(SourceFileId::default(), file_content);
//         //     lexer.collect::<Vec<_>>()
//         // };
//         // write_output(&output_dir.join("module.tokens"), &format!("{:#?}", tokens))?;
//     }

//     for (name, contract) in module.contracts.drain(0..) {
//         let contract_output_dir = output_dir.join(&name);
//         fs::create_dir_all(&contract_output_dir).map_err(ioerr_to_string)?;

//         if targets.contains(&CompilationTarget::Abi) {
//             let file_name = format!("{}_abi.json", &name);
//             write_output(&contract_output_dir.join(file_name), &contract.json_abi)?;
//         }

//         if targets.contains(&CompilationTarget::Yul) {
//             let file_name = format!("{}_ir.yul", &name);
//             write_output(&contract_output_dir.join(file_name), &contract.yul)?;
//         }

//         #[cfg(feature = "solc-backend")]
//         if targets.contains(&CompilationTarget::Bytecode) {
//             let file_name = format!("{}.bin", &name);
//             write_output(&contract_output_dir.join(file_name), &contract.bytecode)?;
//         }
//     }

//     Ok(())
// }

// fn write_output(path: &Path, content: &str) -> Result<(), String> {
//     let mut file = fs::OpenOptions::new()
//         .write(true)
//         .create(true)
//         .truncate(true)
//         .open(path)
//         .map_err(ioerr_to_string)?;
//     file.write_all(content.as_bytes())
//         .map_err(ioerr_to_string)?;
//     Ok(())
// }

// fn ioerr_to_string(error: Error) -> String {
//     format!("{}", error)
// }

// fn verify_nonexistent_or_empty(dir: &Path) -> Result<(), String> {
//     if !dir.exists() || dir.read_dir().map_err(ioerr_to_string)?.next().is_none() {
//         Ok(())
//     } else {
//         Err(format!(
//             "Directory '{}' is not empty. Use --overwrite to overwrite.",
//             dir.display()
//         ))
//     }
// }
