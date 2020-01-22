#![feature(external_doc)]
#![doc(include = "../README.md")]

enum Target {
    YUL,
    EWASM,
    EVM,
}

fn compile(src: &str, target: Target) -> &str {
    ""
}

fn main() {
    println!("Hello, world!");
}
