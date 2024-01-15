use std::sync::{Arc, Mutex};

use crate::db::LanguageServerDatabase;
use crate::handlers::notifications::get_diagnostics;
use crate::server::server_capabilities;
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};
use anyhow::Result;

use log::{info, Level, LevelFilter, Metadata, Record, SetLoggerError};

use lsp_types::{DidChangeWatchedFilesRegistrationOptions, FileSystemWatcher, GlobPattern, InitializeParams, InitializeResult, Registration, TextDocumentItem};


use tower_lsp::{Client, LanguageServer};

async fn register_capabilities(client: Arc<tokio::sync::Mutex<Client>>) -> Result<()> {
    let client = client.lock().await;
    let registration = Registration {
        id: String::from("watch-fe-files"),
        method: String::from("workspace/didChangeWatchedFiles"),
        register_options: Some(serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
            watchers: vec![FileSystemWatcher {
                glob_pattern: GlobPattern::String("**/*.fe".to_string()),
                kind: None,
            }],
        }).unwrap()),
    };
    client.register_capability(vec![registration]).await;

    Ok(())
}

pub struct Backend {
    // pub(crate) sender: Arc<Mutex<Sender<Message>>>,
    pub(crate) client: Arc<tokio::sync::Mutex<Client>>,
    pub(crate) db: Arc<tokio::sync::Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Arc<tokio::sync::Mutex<Workspace>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        initialize_params: InitializeParams,
    ) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        // let initialize_params: InitializeParams = serde_json::from_value(_initialize_params)?;

        let capabilities = server_capabilities();

        let initialize_result = lsp_types::InitializeResult {
            capabilities,
            server_info: Some(lsp_types::ServerInfo {
                name: String::from("fe-language-server"),
                version: Some(String::from(env!("CARGO_PKG_VERSION"))),
            }),
        };
        let _ = self.init_logger(log::Level::Info);
        let workspace = &mut *self.workspace.lock().await;
        let db = &mut *self.db.lock().await;
        let _ = workspace.set_workspace_root(
            db,
            initialize_params
                .root_uri
                .unwrap()
                .to_file_path()
                .ok()
                .unwrap(),
        );
        let client = self.client.clone();
        let _ = register_capabilities(client).await;
        Ok(initialize_result)
    }
    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        // let _ =
        info!("did open: {:?}", params);
        {
            let db = &mut *self.db.lock().await;
            let workspace = &mut *self.workspace.lock().await;
            let input = workspace
                .input_from_file_path(
                    db,
                    params
                        .text_document
                        .uri
                        .to_file_path()
                        .unwrap()
                        .to_str()
                        .unwrap(),
                )
                .unwrap();
            let _ = input.sync(db, None);
        }
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            language_id: params.text_document.language_id,
            version: params.text_document.version,
            text: params.text_document.text,
        })
        .await;
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let db = Arc::new(tokio::sync::Mutex::new(LanguageServerDatabase::default()));
        let workspace = Arc::new(tokio::sync::Mutex::new(Workspace::default()));
        let client = Arc::new(tokio::sync::Mutex::new(client));
        Self {
            client,
            db,
            workspace,
        }
    }

    async fn on_change(&self, params: TextDocumentItem) {
        let client = self.client.lock().await;
        let db = &mut *self.db.lock().await;
        let workspace = &mut *self.workspace.lock().await;
        let diagnostics = get_diagnostics(db, workspace, params.uri.clone())
            .unwrap()
            .into_iter()
            .map(|(uri, diags)| client.publish_diagnostics(uri, diags, None))
            .collect::<Vec<_>>();

        futures::future::join_all(diagnostics).await;
    }

    pub fn init_logger(&self, level: Level) -> Result<(), SetLoggerError> {
        let logger = LspLogger {
            level,
            client: self.client.clone(),
        };
        let static_logger = Box::leak(Box::new(logger));
        log::set_logger(static_logger)?;
        log::set_max_level(LevelFilter::Debug);
        Ok(())
    }
}

pub struct LspLogger {
    level: Level,
    client: Arc<tokio::sync::Mutex<Client>>,
}

impl log::Log for LspLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        let logger = self;
        metadata.level() <= logger.level
    }

    // TODO: investigate performance implications of this
    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let message = format!("{} - {}", record.level(), record.args());
            let level = record.level();
            let client = self.client.clone();
            tokio::task::spawn(async move {
                let client = client.lock().await;
                client
                    .log_message(
                        match level {
                            log::Level::Error => lsp_types::MessageType::ERROR,
                            log::Level::Warn => lsp_types::MessageType::WARNING,
                            log::Level::Info => lsp_types::MessageType::INFO,
                            log::Level::Debug => lsp_types::MessageType::LOG,
                            log::Level::Trace => lsp_types::MessageType::LOG,
                        },
                        message,
                    )
                    .await;
            });
        }
    }

    fn flush(&self) {}
}
