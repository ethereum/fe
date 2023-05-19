#![cfg(feature = "solc-backend")]
use clap::Args;
use fe_common::diagnostics::print_diagnostics;
use fe_proof_service::invariant::Invariant;
use fe_proof_service::ProofClient;

// const DEFAULT_OUTPUT_DIR_NAME: &str = "prove";
// const DEFAULT_INGOT: &str = "main";

#[derive(Args)]
#[clap(about = "Generate specs for the current project")]
pub struct ProveArgs {
    input_path: String,
    #[clap(long, takes_value(true))]
    optimize: Option<bool>,
}

fn single_file_invariants(prove_arg: &ProveArgs) -> Vec<Invariant> {
    let input_path = &prove_arg.input_path;
    let optimize = prove_arg.optimize.unwrap_or(true);

    let mut db = fe_driver::Db::default();
    let content = match std::fs::read_to_string(input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{input_path}`. Error: {err}");
            std::process::exit(1)
        }
        Ok(content) => content,
    };

    match fe_driver::compile_single_file_invariants(&mut db, input_path, &content, optimize) {
        Ok(invariants) => invariants,
        Err(error) => {
            eprintln!("Unable to compile {input_path}.");
            print_diagnostics(&db, &error.0);
            std::process::exit(1)
        }
    }
}

pub fn prove(prove_arg: ProveArgs) {
    let invariants = single_file_invariants(&prove_arg);
    let server = ProofClient::new("10.0.0.144:7878");

    for invariant in invariants {
        let name = invariant.name.clone();
        let status = server.check_invariant(invariant);
        println!("{} (status: {})", &name, &status)
    }
}
