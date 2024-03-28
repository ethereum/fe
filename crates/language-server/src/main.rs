mod backend;
mod capabilities;
mod diagnostics;
mod globals;
mod goto;
mod logger;
mod server;
mod util;

use backend::db::Jar;
use backend::Backend;
use tracing::Level;

use server::Server;

use crate::logger::{handle_log_messages, setup_logger};

#[tokio_macros::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let rx = setup_logger(Level::INFO).unwrap();

    let (message_senders, message_receivers) = server::setup_message_channels();
    let (service, socket) =
        tower_lsp::LspService::build(|client| Server::new(client, message_senders)).finish();
    let server = service.inner();

    let client = server.client.clone();
    let mut backend = Backend::new(client);

    // separate runtime for the backend
    // let backend_runtime = tokio::runtime::Builder::new_multi_thread()
    //     .worker_threads(4)
    //     .enable_all()
    //     .build()
    //     .unwrap();

    // backend_runtime.spawn(backend.handle_streams());

    tokio::select! {
        // setup logging
        _ = handle_log_messages(rx, server.client.clone()) => {},
        // start the server
        _ = tower_lsp::Server::new(stdin, stdout, socket)
            .serve(service) => {}
        // backend
        _ = backend::streams::setup_streams(&mut backend, message_receivers) => {}
    }
}
