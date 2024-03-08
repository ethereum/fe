use crate::handlers::request::{handle_goto_definition, handle_hover};
use crate::workspace::SyncableIngotFileContext;
use futures::TryStreamExt;
use lsp_types::TextDocumentItem;
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::capabilities::server_capabilities;
use crate::db::LanguageServerDatabase;

use crate::diagnostics::get_diagnostics;
use crate::globals::LANGUAGE_ID;
use crate::language_server::MessageChannels;
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};

use log::info;

use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::StreamExt;
use tower_lsp::Client;

pub struct Backend {
    pub(crate) messaging: Arc<Mutex<MessageChannels>>,
    pub(crate) client: Arc<Mutex<Client>>,
    pub(crate) db: LanguageServerDatabase,
    pub(crate) workspace: Workspace,
}

impl Backend {
    pub fn new(client: Arc<Mutex<Client>>, messaging: Arc<Mutex<MessageChannels>>) -> Self {
        let db = LanguageServerDatabase::default();
        let workspace = Workspace::default();

        Self {
            messaging,
            client,
            db,
            workspace,
        }
    }
    pub async fn handle_streams(mut self) {
        info!("setting up streams");
        let workspace = &mut self.workspace;
        let db = &mut self.db;

        let client = self.client.clone();
        let messaging = self.messaging.clone();
        let messaging = messaging.lock().await;

        let mut initialized_stream = BroadcastStream::new(messaging.subscribe_initialize()).fuse();
        let mut shutdown_stream = BroadcastStream::new(messaging.subscribe_shutdown()).fuse();
        let did_open_stream = BroadcastStream::new(messaging.subscribe_did_open()).fuse();
        let did_change_stream = BroadcastStream::new(messaging.subscribe_did_change()).fuse();
        let mut change_stream = tokio_stream::StreamExt::merge(
            did_open_stream.map_ok(|params| TextDocumentItem {
                uri: params.text_document.uri,
                language_id: LANGUAGE_ID.to_string(),
                version: params.text_document.version,
                text: params.text_document.text,
            }),
            did_change_stream.map_ok(|params| TextDocumentItem {
                uri: params.text_document.uri,
                language_id: LANGUAGE_ID.to_string(),
                version: params.text_document.version,
                text: params.content_changes[0].text.clone(),
            }),
        )
        .fuse();
        let mut did_close_stream = BroadcastStream::new(messaging.subscribe_did_close()).fuse();
        let mut did_change_watched_files_stream =
            BroadcastStream::new(messaging.subscribe_did_change_watched_files()).fuse();

        let mut hover_stream = BroadcastStream::new(messaging.subscribe_hover()).fuse();
        let mut goto_definition_stream =
            BroadcastStream::new(messaging.subscribe_goto_definition()).fuse();

        // This is very important! We absolutely need to drop the messaging lock here.
        // TODO: make this more ergonomic and foolproof somehow
        std::mem::drop(messaging);

        info!("streams set up, looping on them now");
        loop {
            tokio::select! {
                Some(result) = initialized_stream.next() => {
                    if let Ok((initialization_params, responder)) = result {
                        info!("initializing language server!");
                        // setup workspace
                        let _ = workspace.set_workspace_root(
                            db,
                            initialization_params
                                .root_uri
                                .unwrap()
                                .to_file_path()
                                .ok()
                                .unwrap(),
                        );

                        let capabilities = server_capabilities();
                        let initialize_result = lsp_types::InitializeResult {
                            capabilities,
                            server_info: Some(lsp_types::ServerInfo {
                                name: String::from("fe-language-server"),
                                version: Some(String::from(env!("CARGO_PKG_VERSION"))),
                            }),
                        };
                        responder.respond(Ok(initialize_result));
                    }
                }
                Some(result) = shutdown_stream.next() => {
                    if let Ok((_, responder)) = result {
                        info!("shutting down language server");
                        responder.respond(Ok(()));
                    }
                }
                Some(Ok(doc)) = change_stream.next() => {
                    info!("change detected: {:?}", doc.uri);
                    on_change(client.clone(), workspace, db, doc).await;
                }
                Some(Ok(params)) = did_close_stream.next() => {
                    let input = workspace
                        .touch_input_from_file_path(
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
                Some(Ok(params)) = did_change_watched_files_stream.next() => {
                    let changes = params.changes;
                    for change in changes {
                        let uri = change.uri;
                        let path = uri.to_file_path().unwrap();

                        match change.typ {
                            lsp_types::FileChangeType::CREATED => {
                                // TODO: handle this more carefully!
                                // this is inefficient, a hack for now
                                let _ = workspace.sync(db);
                                let input = workspace
                                    .touch_input_from_file_path(db, path.to_str().unwrap())
                                    .unwrap();
                                let _ = input.sync(db, None);
                            }
                            lsp_types::FileChangeType::CHANGED => {
                                let input = workspace
                                    .touch_input_from_file_path(db, path.to_str().unwrap())
                                    .unwrap();
                                let _ = input.sync(db, None);
                            }
                            lsp_types::FileChangeType::DELETED => {
                                // TODO: handle this more carefully!
                                // this is inefficient, a hack for now
                                let _ = workspace.sync(db);
                            }
                            _ => {}
                        }
                        // collect diagnostics for the file
                        if change.typ != lsp_types::FileChangeType::DELETED {
                            let text = std::fs::read_to_string(path).unwrap();
                            on_change(
                                self.client.clone(),
                                workspace,
                                db,
                                TextDocumentItem {
                                    uri: uri.clone(),
                                    language_id: LANGUAGE_ID.to_string(),
                                    version: 0,
                                    text,
                                },
                            )
                            .await;
                        }
                    }
                }
                Some(Ok((params, responder))) = hover_stream.next() => {
                    let response = handle_hover(db, workspace, params);
                    responder.respond(response);
                }
                Some(Ok((params, responder))) = goto_definition_stream.next() => {
                    let response = handle_goto_definition(db, workspace, params);
                    responder.respond(response);
                }
            }
        }
    }
}

async fn on_change(
    client: Arc<Mutex<Client>>,
    workspace: &mut Workspace,
    db: &mut LanguageServerDatabase,
    params: TextDocumentItem,
) {
    let client = &mut client.lock().await;
    let diagnostics = {
        let input = workspace
            .touch_input_from_file_path(
                db,
                params
                    .uri
                    .to_file_path()
                    .expect("Failed to convert URI to file path")
                    .to_str()
                    .expect("Failed to convert file path to string"),
            )
            .unwrap();
        let _ = input.sync(db, Some(params.text));
        get_diagnostics(db, workspace, params.uri.clone())
    };

    let diagnostics = diagnostics
        .unwrap()
        .into_iter()
        .map(|(uri, diags)| client.publish_diagnostics(uri, diags, None))
        .collect::<Vec<_>>();

    futures::future::join_all(diagnostics).await;
}
