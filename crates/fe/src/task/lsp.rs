use clap::Args;
use fe_lsp::lsp_server;

#[derive(Args)]
#[clap(about = "Start fe language server protocol")]
pub struct LspArgs {}

pub async fn start_lsp_sever(_args: LspArgs) {
    eprintln!("Started lsp server!!!");
    lsp_server().await;
}
