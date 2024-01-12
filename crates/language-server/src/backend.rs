use std::fmt::Write;

use std::sync::{Arc, Mutex};

use crate::db::LanguageServerDatabase;
use crate::handlers::notifications::get_diagnostics;
use crate::server::server_capabilities;
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};
use anyhow::Result;

use log::{info, Level, Metadata, Record};
use log::{LevelFilter, SetLoggerError};
use lsp_server::Message;
use lsp_types::{InitializeParams, InitializeResult, TextDocumentItem};

use tokio::task;
use tower_lsp::{Client, LanguageServer};

pub struct Backend {
    // pub(crate) sender: Arc<Mutex<Sender<Message>>>,
    pub(crate) client: Client,
    pub(crate) db: Arc<Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Arc<Mutex<Workspace>>,
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
        let workspace = &mut *self.workspace.lock().unwrap();
        let db = &mut *self.db.lock().unwrap();
        let _ = workspace.set_workspace_root(
            db,
            initialize_params
                .root_uri
                .unwrap()
                .to_file_path()
                .ok()
                .unwrap(),
        );
        info!("TESTING");
        // info!("initialized with params: {:?}", debug_params);
        Ok(initialize_result)
    }
    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        // let _ =
        info!("did open: {:?}", params);
        {
            let db = &mut *self.db.lock().unwrap();
            let workspace = &mut *self.workspace.lock().unwrap();
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

use tracing::Subscriber;
use tracing_subscriber::prelude::*;
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::Layer;

impl Backend {
    pub fn new(client: Client) -> Self {
        let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        let workspace = Arc::new(Mutex::new(Workspace::default()));
        Self {
            client,
            db,
            workspace,
        }
    }
    async fn on_change(&self, params: TextDocumentItem) {
        let diagnostics = get_diagnostics(self, params.uri.clone())
            .unwrap()
            .into_iter()
            .map(|(uri, diags)| self.client.publish_diagnostics(uri, diags, None))
            .collect::<Vec<_>>();

        futures::future::join_all(diagnostics).await;
    }


    // pub(crate) fn db(&self) -> &mut LanguageServerDatabase {
    //     let mut db = self.db.lock().unwrap();
    //     &mut *db
    // }

    // pub fn run(&mut self, receiver: Receiver<lsp_server::Message>) -> Result<()> {
    //     info!("Fe Language Server listening...");

    //     // watch the workspace root for changes
    //     self.send(lsp_server::Message::Request(lsp_server::Request::new(
    //         28_716_283.into(),
    //         String::from("client/registerCapability"),
    //         lsp_types::RegistrationParams {
    //             registrations: vec![lsp_types::Registration {
    //                 id: String::from("watch-fe-files"),
    //                 method: String::from("workspace/didChangeWatchedFiles"),
    //                 register_options: Some(
    //                     serde_json::to_value(lsp_types::DidChangeWatchedFilesRegistrationOptions {
    //                         watchers: vec![lsp_types::FileSystemWatcher {
    //                             glob_pattern: lsp_types::GlobPattern::String("**/*.fe".to_string()),
    //                             kind: None, // kind: Some(WatchKind::Create | WatchKind::Change | WatchKind::Delete),
    //                         }],
    //                     })
    //                     .unwrap(),
    //                 ),
    //             }],
    //         },
    //     )))?;

    //     while let Some(msg) = self.next_message(&receiver) {
    //         if let lsp_server::Message::Notification(notification) = &msg {
    //             if notification.method == lsp_types::notification::Exit::METHOD {
    //                 return Ok(());
    //             }
    //         }

    //         let _ = self.handle_message(msg);
    //     }
    //     Ok(())
    // }

    // fn handle_message(&mut self, msg: lsp_server::Message) -> Result<()> {
    //     if let lsp_server::Message::Request(req) = msg {
    //         info!("REQUEST: {:?}", req);

    //         match req.method.as_str() {
    //             // TODO: implement actually useful hover handler
    //             lsp_types::request::HoverRequest::METHOD => handle_hover(self, req)?,
    //             // goto definition
    //             lsp_types::request::GotoDefinition::METHOD => handle_goto_definition(self, req)?,
    //             lsp_types::request::GotoTypeDefinition::METHOD => {
    //                 handle_goto_definition(self, req)?;
    //             }
    //             lsp_types::request::GotoImplementation::METHOD => {
    //                 handle_goto_definition(self, req)?;
    //             }
    //             lsp_types::request::GotoDeclaration::METHOD => handle_goto_definition(self, req)?,
    //             _ => {}
    //         }
    //     } else if let lsp_server::Message::Notification(note) = msg {
    //         // log the notification to the console
    //         info!("NOTIFICATION: {:?}", note);

    //         match note.method.as_str() {
    //             lsp_types::notification::DidOpenTextDocument::METHOD => {
    //                 handle_document_did_open(self, note)?;
    //             }
    //             // TODO: this is currently something of a hack to deal with
    //             // file renames. We should be using the workspace
    //             // "will change" requests instead.
    //             lsp_types::notification::DidCloseTextDocument::METHOD => {
    //                 handle_document_did_close(self, note)?;
    //             }
    //             lsp_types::notification::DidChangeTextDocument::METHOD => {
    //                 handle_document_did_change(self, note)?;
    //             }
    //             lsp_types::notification::DidChangeWatchedFiles::METHOD => {
    //                 handle_watched_file_changes(self, note)?;
    //             }
    //             _ => {}
    //         }
    //     } else if let lsp_server::Message::Response(resp) = msg {
    //         info!("RESPONSE: {:?}", resp);
    //     }

    //     Ok(())
    // }


    pub fn init_logger(&self, level: Level) -> Result<(), SetLoggerError> {
        let logger = LspLogger {
            level,
            client: Arc::new(tokio::sync::Mutex::new(self.client.clone())),
            // sender: self.sender.clone(),
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
    // sender: Arc<Mutex<Sender<Message>>>,
}

impl LspLogger {
    // fn send(&self, msg: Message) -> Result<()> {
    //     let sender = self.sender.lock().unwrap();
    //     sender.send(msg)?;
    //     Ok(())
    // }
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
                // let client = client.lock().unwrap();
                let client = client.lock().await;
                // let client = client.lock().unwrap();
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
                    .await
            });
        }
    }

    fn flush(&self) {}
}
