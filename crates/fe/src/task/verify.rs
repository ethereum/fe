#![cfg(feature = "solc-backend")]
use std::path::Path;

use clap::Args;
use colored::Colorize;
use fe_common::utils::files::BuildFiles;
use fe_driver::{CompiledContract, CompiledModule, Db};
use serde::Deserialize;
use url::Url;

#[derive(Args)]
#[clap(about = "Verify any onchain contract against local available source code.")]
pub struct VerifyArgs {
    #[clap(help("The onchain address of the contract to verify"))]
    contract_address: String,
    #[clap(help("The JSON-RPC URL of the network to verify against"))]
    rpc_url: String,
    #[clap(long, help("Print additional information"))]
    verbose: bool,
    #[clap(long, help("Verify against unoptimized bytecode"))]
    unoptimized: bool,
}

#[derive(Deserialize, Debug)]
struct Response {
    result: String,
}

fn build_ingot(db: &mut Db, optimize: bool) -> Result<CompiledModule, String> {
    let input_path = ".";

    if !Path::new(input_path).exists() {
        return Err(format!("Input directory does not exist: `{input_path}`."));
    }

    let build_files = match BuildFiles::load_fs(input_path) {
        Ok(files) => files,
        Err(err) => return Err(format!("Failed to load project files.\nError: {err}")),
    };

    let compiled_module = match fe_driver::compile_ingot(db, &build_files, true, true, optimize) {
        Ok(module) => module,
        Err(_) => return Err(format!("Unable to compile {input_path}.")),
    };

    Ok(compiled_module)
}

fn validate_args(args: &VerifyArgs) -> Result<(), String> {
    if !args.contract_address.starts_with("0x") {
        return Err(format!(
            "Invalid contract address: {}",
            args.contract_address
        ));
    }

    if !args.contract_address.len() == 42 {
        return Err(format!(
            "Invalid contract address: {}",
            args.contract_address
        ));
    }
    if Url::parse(&args.rpc_url).is_err() {
        return Err(format!("Invalid RPC URL: {}", args.rpc_url));
    }
    Ok(())
}

fn request_bytecode(rpc_url: &str, contract_address: &str) -> Result<String, String> {
    let client = reqwest::blocking::Client::new();
    let res = client
        .post(rpc_url)
        .header("Content-Type", "application/json")
        .body(format!(
            "{{
            \"jsonrpc\": \"2.0\",
            \"id\": 0,
            \"method\": \"eth_getCode\",
            \"params\": [
              \"{contract_address}\",
              \"latest\"
            ]
          }}"
        ))
        .send()
        .map_err(|e| e.to_string())?;

    let response = res.json::<Response>().map_err(|err| err.to_string())?;
    Ok(response.result)
}

pub fn verify(args: VerifyArgs) {
    if let Err(err) = do_work(args) {
        eprintln!("{}", "Failed to verify contract.\n".bold());
        eprintln!("{}", err);
        std::process::exit(1)
    }
}

fn do_work(args: VerifyArgs) -> Result<(), String> {
    validate_args(&args)?;
    let byte_code = request_bytecode(&args.rpc_url, &args.contract_address)?;
    let bytecode = byte_code.strip_prefix("0x").unwrap_or(&byte_code);

    let mut db = fe_driver::Db::default();
    let compiled_module = build_ingot(&mut db, !args.unoptimized)?;
    for (_, contract) in compiled_module.contracts {
        if contract.runtime_bytecode == bytecode {
            print_success(contract, &args, bytecode, &db);
            return Ok(());
        }
    }
    eprintln!("{}", "No contract found with matching bytecode".bold());
    eprintln!("Note: If the contract was deployed with optimization disabled, try verifying with the `--unoptimized` flag.");
    eprintln!(
        "Note: Make sure to use the same compiler version as the one used to deploy the contract."
    );
    Ok(())
}

fn print_success(
    contract: CompiledContract,
    args: &VerifyArgs,
    bytecode: &str,
    db: &fe_driver::Db,
) {
    let bytecode_identifier = if bytecode.len() > 20 {
        let start = &bytecode[..5];
        let end = &bytecode[bytecode.len() - 5..];
        format!("{}..{}", start, end)
    } else {
        // If the string is too short, just print it as it is
        bytecode.to_string()
    };

    let contract_name = contract.origin.name(db);
    let contract_path = contract.origin.span(db).file_id.path(db);

    println!("{}\n", "It's a match!✨".bold());

    println!("{}", "Onchain contract:".bold());
    println!("Address: {}", args.contract_address);
    println!("Bytecode: 0x{bytecode_identifier}");

    println!("\n{}", "Local contract:".bold());
    println!("Contract name: {contract_name}");
    println!("Source file: {contract_path}");
    println!("Bytecode: 0x{bytecode_identifier}");

    if !args.verbose {
        println!(
            "\n{}",
            "Hint: Run with --verbose to see the contract's source code.".bold()
        );
    } else {
        println!("\n{}", "Source code, taken from: {contract_path}".bold());
        let contract_span = contract.origin.span(db);
        let contract_source = contract_span.file_id.content(db);
        let source = &contract_source[contract_span.start..contract_span.end];
        println!("{}", source)
    }
}
