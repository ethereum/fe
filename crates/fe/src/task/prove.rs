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
    name: Option<String>,
    #[clap(long, default_value = "127.0.0.1:7878")]
    server: String,
    #[clap(long, action)]
    rerun: bool,
    #[clap(long, takes_value(true))]
    optimize: Option<bool>,
}

fn single_file_invariants(input_path: &str, optimize: bool) -> Vec<Invariant> {
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

pub fn prove(args: ProveArgs) {
    let input_path = args.input_path;
    let optimize = args.optimize.unwrap_or(true);
    let rerun = args.rerun;
    let invariants = single_file_invariants(&input_path, optimize);
    let server = ProofClient::new(args.server);

    for invariant in invariants {
        let name = invariant.name.clone();

        if let Some(match_name) = &args.name {
            if name.contains(match_name) {
                let status = server.check_invariant(invariant, rerun);
                println!("{0: <20} {1: <10}", &name, &status)
            }
        } else {
            let status = server.check_invariant(invariant, rerun);
            println!("{0:.<20}{1: <10}", &name, &status)
        }
    }
}
