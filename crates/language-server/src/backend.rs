pub(crate) mod db;
mod handlers;
mod helpers;
pub(crate) mod streams;
pub(crate) mod workspace;
use db::LanguageServerDatabase;
use std::sync::Arc;
use tokio::sync::RwLock;
use workspace::Workspace;

use tower_lsp::Client;

pub struct Backend {
    client: Client,
    db: LanguageServerDatabase,
    workspace: Arc<RwLock<Workspace>>,
    workers: tokio::runtime::Runtime,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let db = LanguageServerDatabase::default();
        let workspace = Arc::new(RwLock::new(Workspace::default()));

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(4)
            .enable_all()
            .build()
            .unwrap();
        Self {
            client,
            db,
            workspace,
            workers,
        }
    }
}
