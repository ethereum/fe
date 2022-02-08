#[derive(clap::Args, Debug)]
pub struct NewOpts {
    #[clap(long)]
    lib: bool,

    #[clap(long)]
    bin: bool, // TODO: --contract ?

    #[clap(long)]
    name: Option<String>,
}

pub fn new(_opts: NewOpts) -> anyhow::Result<()> {
    todo!() // XXX
}
