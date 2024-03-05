mod backend;
mod capabilities;
mod db;
mod diagnostics;
mod globals;
mod goto;
mod language_server;
mod logger;
mod oneshot_responder;
mod util;
mod workspace;

use backend::Backend;
// use backend::Backend;
use db::Jar;

use language_server::Server;
use log::info;

mod handlers {
    pub mod request;
}

#[tokio_macros::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(Server::new).finish();
    let server = service.inner();

    let client = server.client.clone();
    let messaging = server.messaging.clone();
    info!("spawning backend");
    let backend = Backend::new(client, messaging);

    tokio::spawn(backend.setup_streams());
    info!("spawning server");
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
