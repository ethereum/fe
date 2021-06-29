/*!

<img src="https://raw.githubusercontent.com/ethereum/fe/master/logo/fe_svg/fe_source.svg" width="150px">

Fe is an emerging smart contract language for the Ethereum blockchain.

[![Build Status](https://github.com/ethereum/fe/workflows/CI/badge.svg)](https://github.com/ethereum/fe/actions)
[![Coverage](https://codecov.io/gh/ethereum/fe/branch/master/graph/badge.svg)](https://codecov.io/gh/ethereum/fe)


## Overview

Fe is a statically typed language for the Ethereum Virtual Machine (EVM). It is inspired by Python and Rust and is easy to learn -- even for developers who are new to the Ethereum ecosystem.

## Features & Goals

* Bounds and overflow checking
* Decidability by limitation of dynamic program behavior
* More precise gas estimation (as a consequence of decidability)
* Static typing
* Pure function support
* Binary fixed-point math
* Restrictions on reentrancy
* Static looping
* Module imports
* Standard library
* Usage of [YUL](https://docs.soliditylang.org/en/latest/yul.html) IR to target both EVM and eWASM
* WASM compiler binaries for enhanced portability and in-browser compilation of
  Fe contracts
* Implementation in a powerful, systems-oriented language (Rust) with strong safety guarantees to reduce risk of compiler bugs

Additional information about design goals and background can be found in the [official announcement](https://snakecharmers.ethereum.org/fe-a-new-language-for-the-ethereum-ecosystem/).

## Language Specification

We aim to provide a full language specification that should eventually be used to formally verify the correctness of the compiler. A work in progress draft of the specification can be found [here](http://fe.ethereum.org/docs/spec/index.html).

## Progress

Fe development is still in its early stages. We have a basic [Roadmap for 2021](https://notes.ethereum.org/LVhaTF30SJOpkbG1iVw1jg) that we want to follow. We generally try to drive the development by working through real world use cases. Our next goal is to provide a working Uniswap implementation in Fe which will help us to advance and form the language.

Fe had its first alpha release January 2021 and is now following a monthly release cycle.

## Getting started

- [Build the compiler](https://github.com/ethereum/fe/blob/master/docs/build.md)
- [Or download the binary release](https://github.com/ethereum/fe/releases)

To compile Fe code:

1. Run `fe path/to/fe_source.fe`
2. Fe creates a directory `output` in the current working directory that contains the compiled binary and abi.

Run `fe --help` to explore further options.

## Examples

The following is a simple contract implemented in Fe.

```fe
type BookMsg = bytes[100]

contract GuestBook:
    pub guest_book: Map<address, BookMsg>

    event Signed:
        book_msg: BookMsg

    pub def sign(book_msg: BookMsg):
        self.guest_book[msg.sender] = book_msg

        emit Signed(book_msg=book_msg)

    pub def get_msg(addr: address) -> BookMsg:
        return self.guest_book[addr].to_mem()
```

A lot more working examples can be found in our [test fixtures directory](https://github.com/ethereum/fe/tree/master/compiler/tests/fixtures).

The most advanced example that we can provide at this point is a fully working [ERC20 implementation](https://github.com/ethereum/fe/blob/master/compiler/tests/fixtures/demos/erc20_token.fe).

## Community

- Twitter: [@official_fe](https://twitter.com/official_fe)
- Chat: [Discord](https://discord.gg/ywpkAXFjZH)

*/

use std::fs;
use std::io::{Error, Write};
use std::path::Path;

use clap::{arg_enum, values_t, App, Arg};

use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use fe_common::panic::install_panic_hook;
use fe_driver::CompiledModule;

const DEFAULT_OUTPUT_DIR_NAME: &str = "output";
const VERSION: &str = env!("CARGO_PKG_VERSION");

arg_enum! {
    #[derive(PartialEq, Debug)]
    pub enum CompilationTarget {
        Abi,
        Ast,
        LoweredAst,
        Bytecode,
        Tokens,
        Yul,
    }
}

pub fn main() {
    install_panic_hook();

    let matches = App::new("Fe")
        .version(VERSION)
        .about("Compiler for the Fe language")
        .arg(
            Arg::with_name("input")
                .help("The input source file to use e.g erc20.fe")
                .index(1)
                .required(true),
        )
        .arg(
            Arg::with_name("output-dir")
                .short("o")
                .long("output-dir")
                .help("The directory to store the compiler output e.g /tmp/output")
                .takes_value(true)
                .default_value(DEFAULT_OUTPUT_DIR_NAME),
        )
        .arg(
            Arg::with_name("emit")
                .short("e")
                .long("emit")
                .help("Comma separated compile targets e.g. -e=bytecode,yul")
                .possible_values(&["abi", "bytecode", "ast", "tokens", "yul", "loweredAst"])
                .default_value("abi,bytecode")
                .use_delimiter(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("overwrite")
                .long("overwrite")
                .help("Overwrite contents of output directory`"),
        )
        .arg(
            Arg::with_name("optimize")
                .long("optimize")
                .help("Enables the Yul optimizer`")
                .possible_values(&["true", "false"])
                .default_value("true")
                .use_delimiter(false)
                .takes_value(true),
        )
        .get_matches();

    let input_file = matches.value_of("input").unwrap();
    let output_dir = matches.value_of("output-dir").unwrap();
    let overwrite = matches.is_present("overwrite");
    let optimize = matches.value_of("optimize") == Some("true");
    let targets =
        values_t!(matches.values_of("emit"), CompilationTarget).unwrap_or_else(|e| e.exit());
    let with_bytecode = targets.contains(&CompilationTarget::Bytecode);
    #[cfg(not(feature = "solc-backend"))]
    if with_bytecode {
        eprintln!("Warning: bytecode output requires 'solc-backend' feature. Try `cargo build --release --features solc-backend`. Skipping.");
    }

    let mut files = FileStore::new();
    let file = files.load_file(input_file).map_err(ioerr_to_string);
    if let Err(err) = file {
        eprintln!("Failed to load file: `{}`. Error: {}", input_file, err);
        std::process::exit(1);
    }
    let (content, id) = file.unwrap();

    let compiled_module = match fe_driver::compile(&content, id, with_bytecode, optimize) {
        Ok(module) => module,
        Err(error) => {
            eprintln!("Unable to compile {}.", input_file);
            print_diagnostics(&error.0, &files);
            std::process::exit(1)
        }
    };
    match write_compiled_module(compiled_module, &content, &targets, &output_dir, overwrite) {
        Ok(_) => println!("Compiled {}. Outputs in `{}`", input_file, output_dir),
        Err(err) => {
            eprintln!(
                "Failed to write output to directory: `{}`. Error: {}",
                output_dir, err
            );
            std::process::exit(1)
        }
    }
}

fn write_compiled_module(
    mut module: CompiledModule,
    file_content: &str,
    targets: &[CompilationTarget],
    output_dir: &str,
    overwrite: bool,
) -> Result<(), String> {
    let output_dir = Path::new(output_dir);
    if output_dir.is_file() {
        return Err(format!(
            "A file exists at path `{}`, the location of the output directory. Refusing to overwrite.",
            output_dir.display()
        ));
    }

    if !overwrite {
        verify_nonexistent_or_empty(output_dir)?;
    }

    fs::create_dir_all(output_dir).map_err(ioerr_to_string)?;

    if targets.contains(&CompilationTarget::Ast) {
        write_output(&output_dir.join("module.ast"), &module.src_ast)?;
    }

    if targets.contains(&CompilationTarget::LoweredAst) {
        write_output(&output_dir.join("lowered_module.ast"), &module.lowered_ast)?;
    }

    if targets.contains(&CompilationTarget::Tokens) {
        let tokens = {
            let lexer = fe_parser::lexer::Lexer::new(file_content);
            lexer.collect::<Vec<_>>()
        };
        write_output(&output_dir.join("module.tokens"), &format!("{:#?}", tokens))?;
    }

    for (name, contract) in module.contracts.drain() {
        let contract_output_dir = output_dir.join(&name);
        fs::create_dir_all(&contract_output_dir).map_err(ioerr_to_string)?;

        if targets.contains(&CompilationTarget::Abi) {
            let file_name = format!("{}_abi.json", &name);
            write_output(&contract_output_dir.join(file_name), &contract.json_abi)?;
        }

        if targets.contains(&CompilationTarget::Yul) {
            let file_name = format!("{}_ir.yul", &name);
            write_output(&contract_output_dir.join(file_name), &contract.yul)?;
        }

        #[cfg(feature = "solc-backend")]
        if targets.contains(&CompilationTarget::Bytecode) {
            let file_name = format!("{}.bin", &name);
            write_output(&contract_output_dir.join(file_name), &contract.bytecode)?;
        }
    }

    Ok(())
}

fn write_output(path: &Path, content: &str) -> Result<(), String> {
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .map_err(ioerr_to_string)?;
    file.write_all(content.as_bytes())
        .map_err(ioerr_to_string)?;
    Ok(())
}

fn ioerr_to_string(error: Error) -> String {
    format!("{}", error)
}

fn verify_nonexistent_or_empty(dir: &Path) -> Result<(), String> {
    if !dir.exists() || dir.read_dir().map_err(ioerr_to_string)?.next().is_none() {
        Ok(())
    } else {
        Err(format!(
            "Directory '{}' is not empty. Use --overwrite to overwrite.",
            dir.display()
        ))
    }
}
