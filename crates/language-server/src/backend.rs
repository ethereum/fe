use std::sync::Arc;
use tokio::sync::Mutex;

use crate::db::LanguageServerDatabase;

use crate::language_server::Server;
use crate::workspace::Workspace;
use anyhow::Result;
use log::info;
use lsp_types::{
    DidChangeWatchedFilesRegistrationOptions, FileSystemWatcher, GlobPattern, Registration,
};
use tokio_stream::wrappers::BroadcastStream;
use tower_lsp::Client;
use tokio_stream::StreamExt;

pub struct Backend {
    // pub(crate) client: &'a Client,
    pub(crate) client: Arc<Mutex<Client>>,
    pub(crate) db: LanguageServerDatabase,
    pub(crate) workspace: Workspace,
}

impl Backend {
    pub fn new(client: Arc<Mutex<Client>>, server: &Server) -> Self {
        // let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        // let workspace = Arc::new(Mutex::new(Workspace::default()));
        // let client = Arc::new(tokio::sync::Mutex::new(client));
        let workspace = Workspace::default();
        let db = LanguageServerDatabase::default();
        let backend = Self {
            client,
            db,
            workspace,
        };

        //subscribe to server initialize event
        let mut stream = BroadcastStream::new(server.dispatch.initialize_tx.subscribe());
        tokio::spawn(async move {
            while let Some(event) = stream.next().await {
                match event {
                    Ok(params) => {
                        info!("initialize event received: {:?}", params);
                        // Handle the event here
                    }
                    Err(e) => {
                        eprintln!("Error receiving event: {:?}", e);
                        // Handle the error here
                    }
                }
            }
        });

        // server.dispatch.initialize_rx.resubscribe(move |params| {
        //     info!("initialize event received: {:?}", params);
        // });

        backend
    }
}
