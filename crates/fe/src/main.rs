use clap::Parser;
use driver::Options;

fn main() {
    let opts = Options::parse();
    driver::run(&opts);
}
