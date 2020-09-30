use std::fs;
use std::io::{
    Error,
    ErrorKind,
    Result,
    Write,
};
use std::path::Path;

use clap::{
    arg_enum,
    values_t,
    App,
    Arg,
};

const DEFAULT_OUTPUT_DIR_NAME: &str = "output";
const VERSION: &str = env!("CARGO_PKG_VERSION");

arg_enum! {
    #[derive(PartialEq, Debug)]
    pub enum CompilationTarget {
        Bytecode,
        Yul,
    }
}

pub fn main() {
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
                .help("The directory to store the compiler output e.g /tmp/output")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("emit")
                .short("e")
                .help("Comma seperated compile targets e.g. -e=bytecode,yul")
                .default_value("bytecode")
                .use_delimiter(true)
                .takes_value(true),
        )
        .get_matches();

    let input_file = matches.value_of("input").unwrap();

    let output_dir = matches
        .value_of("output-dir")
        .unwrap_or(DEFAULT_OUTPUT_DIR_NAME);

    let targets =
        values_t!(matches.values_of("emit"), CompilationTarget).unwrap_or_else(|e| e.exit());

    match compile(input_file, output_dir, targets) {
        Ok(_) => println!("Compiled {}. Outputs in {}", input_file, output_dir),
        Err(err) => println!("Unable to compile {}. Error: {}", input_file, err),
    }
}

fn verify_nonexistent_or_empty(dir: &Path) -> Result<()> {
    if !dir.exists() || dir.read_dir()?.next().is_none() {
        Ok(())
    } else {
        Err(Error::new(
            ErrorKind::Other,
            format!("Directory '{}' is not empty", dir.display()),
        ))
    }
}

fn compile(src_file: &str, output_dir: &str, targets: Vec<CompilationTarget>) -> Result<()> {
    let output_dir = Path::new(output_dir);

    verify_nonexistent_or_empty(output_dir)?;
    std::fs::create_dir_all(output_dir)?;

    let contents = fs::read_to_string(src_file)?;

    let output = fe_compiler::evm::compile(&contents)
        .map_err(|e| Error::new(ErrorKind::Other, format!("{:?}", e)))?;

    for target in targets {
        match target {
            CompilationTarget::Bytecode => {
                let mut file_bytecode = fs::File::create(output_dir.join("out.bin"))?;
                file_bytecode.write_all(output.bytecode.as_bytes())?;
            }
            CompilationTarget::Yul => {
                let mut file_yul = fs::File::create(output_dir.join("out.yul"))?;
                file_yul.write_all(output.yul.as_bytes())?;
            }
        }
    }

    Ok(())
}
