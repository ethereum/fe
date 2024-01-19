mod backend;
mod capabilities;
mod db;
mod diagnostics;
mod globals;
mod goto;
mod language_server;
mod logger;
mod util;
mod workspace;

use backend::Backend;
use db::Jar;
mod handlers {
    pub mod notifications;
    pub mod request;
}

#[tokio_macros::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(Backend::new).finish();
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
