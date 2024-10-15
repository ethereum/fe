use clap::{Parser, Subcommand};

/// Language Server Protocol (LSP) Server
#[derive(Parser, Debug)]
#[command(name = "fe-analyzer")]
#[command(author = "Your Name <you@example.com>")]
#[command(version = "1.0")]
#[command(about = "LSP server for the Fe language", long_about = None)]
pub struct CliArgs {
    /// Choose the communication method
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Start the LSP server with a TCP listener
    Tcp(TcpArgs),
}

#[derive(Parser, Debug)]
pub struct TcpArgs {
    /// Port to listen on (default: 4242)
    #[arg(short, long, default_value_t = 4242)]
    pub port: u16,
}
