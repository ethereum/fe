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
// use backend::Backend;
use db::Jar;
use language_server::Server;
mod handlers {
    pub mod notifications;
    pub mod request;
}



#[tokio_macros::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(Server::new).finish();

    let server = service.inner();

    let backend = Backend::new(server.client.clone(), server);
    backend.setup_streams();

    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
