use std::sync::Arc;

use lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions,
    DidCloseTextDocumentParams, FileSystemWatcher, GlobPattern, InitializeParams, InitializeResult,
    Registration,
};

use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

pub(crate) struct Server {
    pub(crate) dispatch: LspChannels,
    pub(crate) client: Arc<tokio::sync::Mutex<Client>>,
}

impl Server {
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
        client.register_capability(vec![registration]).await
    }

    pub(crate) fn new(client: Client) -> Self {
        let dispatch = LspChannels::new();
        let client = Arc::new(tokio::sync::Mutex::new(client));
        Self { dispatch, client }
    }
}

#[language_server_macros::dispatcher]
#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(&self, initialize_params: InitializeParams) -> Result<InitializeResult> {
        let rx = self.dispatch.dispatch_initialize(initialize_params);

        // setup logging
        let _ = self.init_logger(log::Level::Info);

        // register watchers
        let _ = self.register_watchers().await;

        rx.await.unwrap()
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        self.dispatch.dispatch_did_open(params);
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        self.dispatch.dispatch_did_change(params);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.dispatch.dispatch_did_close(params);
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        self.dispatch.dispatch_did_change_watched_files(params);
    }
}
