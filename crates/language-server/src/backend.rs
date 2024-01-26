use std::sync::Arc;
use std::sync::{Mutex, MutexGuard};
// use tokio::sync::{Mutex, MutexGuard};

use crate::db::LanguageServerDatabase;

use crate::workspace::Workspace;
use anyhow::Result;
use lsp_types::{
    DidChangeWatchedFilesRegistrationOptions, FileSystemWatcher, GlobPattern, Registration,
};
use tower_lsp::Client;

pub struct Backend {
    pub(crate) client: Arc<tokio::sync::Mutex<Client>>,
    pub(crate) db: Arc<Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Arc<Mutex<Workspace>>,
}

impl Backend {
    // pub(crate) fn db(&self) -> MutexGuard<LanguageServerDatabase> {
    //     self.db.lock().unwrap()
    // }

    // pub(crate) fn workspace(&self) -> MutexGuard<Workspace> {
    //     self.workspace.lock().unwrap()
    // }

    pub fn new(client: Client) -> Self {
        let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        let workspace = Arc::new(Mutex::new(Workspace::default()));
        let client = Arc::new(tokio::sync::Mutex::new(client));
        Self {
            client,
            db,
            workspace,
        }
    }
    pub(crate) async fn register_watchers(&self) -> Result<()> {
        let client = self.client.lock().await;
        let registration = Registration {
            id: String::from("watch-fe-files"),
            method: String::from("workspace/didChangeWatchedFiles"),
            register_options: Some(
                serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                    watchers: vec![FileSystemWatcher {
                        glob_pattern: GlobPattern::String("**/*.fe".to_string()),
                        kind: None,
                    }],
                })
                .unwrap(),
            ),
        };
        Ok(client.register_capability(vec![registration]).await?)
    }
}
