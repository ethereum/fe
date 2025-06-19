use clap::Parser;
use driver::Options;

fn main() {
    let opts = Options::parse();

    // Initialize tracing with ERROR as default level
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("error")),
        )
        .init();

    driver::run(&opts);
}
