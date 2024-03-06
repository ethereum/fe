use std::sync::Arc;

use log::{error, info};
use lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions,
    DidCloseTextDocumentParams, FileSystemWatcher, GlobPattern, InitializeParams, InitializeResult,
    Registration,
};

use crate::oneshot_responder::OneshotResponder;

use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

pub(crate) struct Server {
    pub(crate) messaging: Arc<tokio::sync::Mutex<MessageChannels>>,
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
        let messaging = Arc::new(tokio::sync::Mutex::new(MessageChannels::new()));
        let client = Arc::new(tokio::sync::Mutex::new(client));

        Self { messaging, client }
    }
}

#[language_server_macros::message_channels(MessageChannels)]
#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(&self, initialize_params: InitializeParams) -> Result<InitializeResult> {
        // forward the initialize request to the messaging system
        let messaging = self.messaging.lock().await;
        let rx = messaging.send_initialize(initialize_params);
        info!("awaiting initialization result");
        let initialize_result = rx.await.unwrap();

        // register file watchers
        let _ = self.register_watchers().await;
        info!("registered watchers");

        info!("received initialization result");
        initialize_result
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        let messaging = self.messaging.lock().await;
        messaging.send_did_open(params);
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        let messaging = self.messaging.lock().await;
        messaging.send_did_change(params);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let messaging = self.messaging.lock().await;
        messaging.send_did_close(params);
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let messaging = self.messaging.lock().await;
        messaging.send_did_change_watched_files(params);
    }

    async fn hover(&self, params: lsp_types::HoverParams) -> Result<Option<lsp_types::Hover>> {
        let messaging = self.messaging.lock().await;
        let rx = messaging.send_hover(params);
        rx.await.unwrap()
    }

    async fn goto_definition(
        &self,
        params: lsp_types::GotoDefinitionParams,
    ) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
        let messaging = self.messaging.lock().await;
        let rx = messaging.send_goto_definition(params);
        rx.await.unwrap()
    }
}
