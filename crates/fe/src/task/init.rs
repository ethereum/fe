use clap::Args;


#[derive(Args)]
pub struct InitArg {
    name: String,
}

pub fn init(arg: InitArg) {
    println!("{}", arg.name);
}