pub(crate) mod db;
pub(crate) mod workspace;
use db::LanguageServerDatabase;
use std::sync::Arc;
use tokio::sync::RwLock;
use workspace::Workspace;

use tower_lsp::Client;

pub struct Backend {
    pub(super) client: Client,
    pub(super) db: LanguageServerDatabase,
    pub(super) workspace: Arc<RwLock<Workspace>>,
    pub(super) workers: tokio::runtime::Runtime,
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
