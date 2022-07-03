use fe_lsp::lsp_server;

#[tokio::main]
async fn main() {
    lsp_server().await;
}
