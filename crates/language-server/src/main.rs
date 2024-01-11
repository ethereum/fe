mod db;
mod diagnostics;
mod goto;
mod server;
// mod state;
mod backend;
mod util;
mod workspace;

use backend::Backend;
use db::Jar;
mod handlers {
    pub mod notifications;
    pub mod request;
}

// use server::run_server;

// fn main() {
//     // let _ = run_server();
// }
#[tokio_macros::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::build(Backend::new).finish();
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
