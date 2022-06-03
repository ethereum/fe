use clap::Args;
const DEFAULT_OUTPUT_DIR_NAME: &str = "output";

#[derive(Args)]
pub struct CompileArg {
    input: String,
    #[clap(short, long, default_value = DEFAULT_OUTPUT_DIR_NAME)]
    output_dir: String,
}

pub fn compile(arg: CompileArg) {
    println!("{}", arg.input);
}