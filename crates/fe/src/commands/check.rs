use std::path::PathBuf;

#[derive(clap::Args, Debug)]
pub struct CheckOpts {
    #[clap(short, long, parse(from_os_str))]
    file: Option<PathBuf>,
}

pub fn check(_opts: CheckOpts) -> anyhow::Result<()> {
    todo!() // XXX
}
