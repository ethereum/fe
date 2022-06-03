
use clap::Args;

#[derive(Args)]
pub struct CheckArg {
    input: String,
}

pub fn check(arg: CheckArg) {
    println!("{}", arg.input);
}