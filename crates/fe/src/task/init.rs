use clap::Args;


#[derive(Args)]
pub struct InitArg {
    name: String,
}

pub fn init(arg: InitArg) {
    println!("Init new project with name = {}", arg.name);
    println!("Command not support yet!!!");
}