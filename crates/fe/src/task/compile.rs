use std::path::Path;

use clap::{Args, ArgEnum};
const DEFAULT_OUTPUT_DIR_NAME: &str = "output";

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ArgEnum, Debug)]
enum Emit{
    Abi,
    Ast,
    LoweredAst,
    Bytecode,
    Tokens,
    Yul
}

#[derive(Args)]
pub struct CompileArg {
    input_path: String,
    #[clap(short, long, default_value = DEFAULT_OUTPUT_DIR_NAME)]
    output_dir: String,
    
    #[clap(arg_enum, use_value_delimiter=true, long, short, default_value="abi,bytecode")]
    emit: Vec<Emit>,

    #[clap(long)]
    overwrite: bool,
    #[clap(long)]
    optimize: bool   
}

fn compile_single_file(file_name: &str) {
    println!("Hello world")
}

fn compile_ingot() {

}

pub fn compile(compile_arg: CompileArg) {
    if Path::new(&compile_arg.input_path).is_file() {
        compile_single_file(&compile_arg.input_path);
    } else {
        compile_ingot();
    }
}

