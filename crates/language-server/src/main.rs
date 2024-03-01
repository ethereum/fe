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

use std::sync::Arc;

use backend::Backend;
// use backend::Backend;
use db::Jar;
use futures::future::join_all;
use language_server::{LspChannels, Server};
use log::info;
use tower_lsp::Client;
mod handlers {
    pub mod notifications;
    pub mod request;
}

#[tokio_macros::main]
async fn main() {
    // let runtime = tokio::runtime::Builder::new_multi_thread()
    //     .worker_threads(2)
    //     .enable_all()
    //     .build()
    //     .unwrap();

    // let runtime2 = tokio::runtime::Builder::new_multi_thread()
    //     .worker_threads(2)
    //     .enable_all()
    //     .build()
    //     .unwrap();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(Server::new).finish();
    let server = service.inner();
    // server.init_logger(log::Level::Info).unwrap();
    // info!("initialized logger");

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
