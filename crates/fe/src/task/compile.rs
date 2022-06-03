use clap::Args;
const DEFAULT_OUTPUT_DIR_NAME: &str = "output";

#[derive(Args)]
pub struct CompileArg {
    input_path: String,
    #[clap(short, long, default_value = DEFAULT_OUTPUT_DIR_NAME)]
    output_dir: String,
}

pub fn compile(compile_arg: CompileArg) {
    println!("{}", compile_arg.input_path);
}
