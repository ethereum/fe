#![feature(external_doc)]
#![doc(include = "../README.md")]
extern crate vyper_compiler as compiler;
extern crate vyper_parser as parser;
#[macro_use]
extern crate clap;
use clap::App;

use std::fs;
use std::convert::TryFrom;

enum Target {
    Abi,
    Yul,
    Ewasm,
    Evm,
}

impl TryFrom<&str> for Target {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Target, Self::Error> {
        match s {
            "abi" => Ok(Target::Abi),
            "yul" => Ok(Target::Yul),
            "ewasm" => Ok(Target::Ewasm),
            "evm" => Ok(Target::Evm),
            _ => Err("Target not recognized.")
        }
    }
}

fn compile(src: &str, target: Target) -> Result<String, compiler::errors::CompileError> {
    match target {
        Target::Abi => compiler::abi::compile(src),
        Target::Yul => compiler::yul::compile(src),
        Target::Evm => compiler::evm::compile(src),
        _ => Err(compiler::errors::CompileError::static_str("Target is not supported."))
    }
}

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let src = fs::read_to_string(matches.value_of("INPUT").unwrap()).unwrap();
    let targets = matches
        .values_of("targets")
        .unwrap()
        .map(|t| Target::try_from(t)).collect::<Result<Vec<Target>, &str>>()
        .unwrap();

    for target in targets {
        print!("{}", compile(&src, target).unwrap())
    }
}
