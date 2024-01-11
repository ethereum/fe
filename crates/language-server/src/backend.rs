use std::sync::{Arc, Mutex, MutexGuard};

use crate::db::LanguageServerDatabase;
use crate::server::server_capabilities;
use crate::workspace::Workspace;
use anyhow::Result;
use crossbeam_channel::{Receiver, Sender};
use log::{info, Level, Metadata, Record};
use log::{LevelFilter, SetLoggerError};
use lsp_server::Message;
use lsp_types::{InitializeParams, InitializeResult};
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::handlers::notifications::{handle_document_did_change, handle_watched_file_changes, handle_document_did_close};
use crate::handlers::request::handle_goto_definition;
use crate::handlers::{notifications::handle_document_did_open, request::handle_hover};

pub struct Backend {
    // pub(crate) sender: Arc<Mutex<Sender<Message>>>,
    client: Client,
    pub(crate) db: Arc<Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Workspace,

}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        // let initialize_params: InitializeParams = serde_json::from_value(_initialize_params)?;

        let capabilities = server_capabilities();

        let initialize_result = lsp_types::InitializeResult {
            capabilities,
            server_info: Some(lsp_types::ServerInfo {
                name: String::from("fe-language-server"),
                version: Some(String::from(env!("CARGO_PKG_VERSION"))),
            }),
        };
        Ok(initialize_result)
    }
    fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        Self {
            client,
            db,
            workspace: Workspace::default(),
        }
    }

    fn db(&self) -> MutexGuard<LanguageServerDatabase> {
        self.db.lock().unwrap()
    }

    pub fn run(&mut self, receiver: Receiver<lsp_server::Message>) -> Result<()> {
        info!("Fe Language Server listening...");

        // watch the workspace root for changes
        self.send(lsp_server::Message::Request(lsp_server::Request::new(
            28_716_283.into(),
            String::from("client/registerCapability"),
            lsp_types::RegistrationParams {
                registrations: vec![lsp_types::Registration {
                    id: String::from("watch-fe-files"),
                    method: String::from("workspace/didChangeWatchedFiles"),
                    register_options: Some(
                        serde_json::to_value(lsp_types::DidChangeWatchedFilesRegistrationOptions {
                            watchers: vec![lsp_types::FileSystemWatcher {
                                glob_pattern: lsp_types::GlobPattern::String("**/*.fe".to_string()),
                                kind: None, // kind: Some(WatchKind::Create | WatchKind::Change | WatchKind::Delete),
                            }],
                        })
                        .unwrap(),
                    ),
                }],
            },
        )))?;

        while let Some(msg) = self.next_message(&receiver) {
            if let lsp_server::Message::Notification(notification) = &msg {
                if notification.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }
            }

            let _ = self.handle_message(msg);
        }
        Ok(())
    }


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
            client: Arc::new(Mutex::new(self.client.clone())),
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
    client: Arc<Mutex<Client>>,
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

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let message = format!("{} - {}", record.level(), record.args());
            let client = self.client.lock().unwrap();
            let _ = client.log_message(
                
               match record.level() {
                            Level::Error => lsp_types::MessageType::ERROR,
                            Level::Warn => lsp_types::MessageType::WARNING,
                            Level::Info => lsp_types::MessageType::INFO,
                            Level::Debug => lsp_types::MessageType::LOG,
                            Level::Trace => lsp_types::MessageType::LOG,
                        },
            message
            );
        }
    }

    fn flush(&self) {}
}
