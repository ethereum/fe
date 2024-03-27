use crate::handlers::request::{handle_goto_definition, handle_hover};

use crate::workspace::SyncableIngotFileContext;

use common::InputDb;
use fork_stream::StreamExt as _;
use futures_batch::ChunksTimeoutStreamExt;
use fxhash::FxHashSet;

use futures::StreamExt;
use futures_concurrency::prelude::*;
use lsp_types::TextDocumentItem;
use salsa::{ParallelDatabase, Snapshot};
use tokio_stream::wrappers::UnboundedReceiverStream;

use std::sync::Arc;
use tokio::sync::RwLock;

use crate::capabilities::server_capabilities;
use crate::db::LanguageServerDatabase;

use crate::diagnostics::get_diagnostics;
use crate::globals::LANGUAGE_ID;
use crate::language_server::MessageReceivers;
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};

use tracing::info;

// use tokio_stream::StreamExt;

use tower_lsp::Client;

pub struct Backend {
    pub(crate) messaging: MessageReceivers,
    pub(crate) client: Client,
    pub(crate) db: LanguageServerDatabase,
    pub(crate) workspace: Arc<RwLock<Workspace>>,
    workers: tokio::runtime::Runtime,
}

impl Backend {
    pub fn new(client: Client, messaging: MessageReceivers) -> Self {
        let db = LanguageServerDatabase::default();
        let workspace = Arc::new(RwLock::new(Workspace::default()));

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(4)
            .enable_all()
            .build()
            .unwrap();
        Self {
            messaging,
            client,
            db,
            workspace,
            workers,
        }
    }
    pub async fn handle_streams(mut self) {
        info!("setting up streams");
        let workspace = self.workspace.clone();
        let db = &mut self.db;

        let client = self.client.clone();
        let messaging = self.messaging;

        let mut initialized_stream = messaging.initialize_stream.fuse();
        let mut shutdown_stream = messaging.shutdown_stream.fuse();
        let did_change_watched_files_stream = messaging.did_change_watched_files_stream.fork();

        let flat_did_change_watched_files = did_change_watched_files_stream
            .map(|params| futures::stream::iter(params.changes))
            .flatten()
            .fork();

        let did_change_watched_file_stream =
            flat_did_change_watched_files.clone().filter(|change| {
                let change_type = change.typ;
                Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::CHANGED) })
            });

        let did_create_watched_file_stream =
            flat_did_change_watched_files.clone().filter(|change| {
                let change_type = change.typ;
                Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::CREATED) })
            });

        let mut did_delete_watch_file_stream = flat_did_change_watched_files
            .clone()
            .filter(|change| {
                let change_type = change.typ;
                Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::DELETED) })
            })
            .fuse();

        let did_open_stream = messaging.did_open_stream.fuse();
        let did_change_stream = messaging.did_change_stream.fuse();
        let mut change_stream = (
            did_change_watched_file_stream.map(|change| {
                let uri = change.uri;
                let path = uri.to_file_path().unwrap();
                let text = std::fs::read_to_string(path).unwrap();
                TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text,
                }
            }),
            did_create_watched_file_stream.map(|change| {
                let uri = change.uri;
                let path = uri.to_file_path().unwrap();
                let text = std::fs::read_to_string(path).unwrap();
                TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text,
                }
            }),
            did_open_stream.map(|params| TextDocumentItem {
                uri: params.text_document.uri,
                language_id: LANGUAGE_ID.to_string(),
                version: params.text_document.version,
                text: params.text_document.text,
            }),
            did_change_stream.map(|params| TextDocumentItem {
                uri: params.text_document.uri,
                language_id: LANGUAGE_ID.to_string(),
                version: params.text_document.version,
                text: params.content_changes[0].text.clone(),
            }),
        )
            .merge()
            .fuse();

        let (tx_needs_diagnostics, rx_needs_diagnostics) = tokio::sync::mpsc::unbounded_channel();

        let mut diagnostics_stream = UnboundedReceiverStream::from(rx_needs_diagnostics)
            .chunks_timeout(500, std::time::Duration::from_millis(30))
            .fuse();

        let mut hover_stream = messaging.hover_stream.fuse();
        let mut goto_definition_stream = messaging.goto_definition_stream.fuse();

        info!("streams set up, looping on them now");
        loop {
            tokio::select! {
                Some(result) = initialized_stream.next() => {
                    let (initialization_params, responder) = result;
                    info!("initializing language server!");

                    let root =
                        initialization_params
                            .root_uri
                            .unwrap()
                            .to_file_path()
                            .ok()
                            .unwrap();

                    let mut workspace = self.workspace.write().await;
                    let _ = workspace.set_workspace_root(
                        db,
                        &root
                    );
                    let _ = workspace.load_std_lib(db, &root);
                    let _ = workspace.sync(db);

                    let capabilities = server_capabilities();
                    let initialize_result = lsp_types::InitializeResult {
                        capabilities,
                        server_info: Some(lsp_types::ServerInfo {
                            name: String::from("fe-language-server"),
                            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
                        }),
                    };
                    let _ = responder.send(Ok(initialize_result));
                }
                Some(result) = shutdown_stream.next() => {
                    let (_, responder) = result;
                    info!("shutting down language server");
                    let _ = responder.send(Ok(()));
                }
                Some(deleted) = did_delete_watch_file_stream.next() => {
                    let path = deleted.uri.to_file_path().unwrap();
                    info!("file deleted: {:?}", path);
                    let path = path.to_str().unwrap();
                    let workspace = workspace.clone();
                    let _ = workspace.write().await.remove_input_for_file_path(db, path);
                    let _ = tx_needs_diagnostics.send(path.to_string());
                }
                Some(doc) = change_stream.next() => {
                    info!("change detected: {:?}", doc.uri);
                    let path_buf = doc.uri.to_file_path().unwrap();
                    let path = path_buf.to_str().unwrap();
                    let contents = Some(doc.text);
                    update_input(workspace.clone(), db, path, contents).await;
                    let _ = tx_needs_diagnostics.send(path.to_string());
                }
                Some(files_need_diagnostics) = diagnostics_stream.next() => {
                    info!("files need diagnostics: {:?}", files_need_diagnostics);
                    let mut ingots_need_diagnostics = FxHashSet::default();
                    for file in files_need_diagnostics {
                        let workspace = workspace.clone();
                        let workspace = workspace.read().await;
                        let ingot = workspace.get_ingot_for_file_path(&file).unwrap();
                        ingots_need_diagnostics.insert(ingot);
                    }

                    info!("ingots need diagnostics: {:?}", ingots_need_diagnostics);
                    for ingot in ingots_need_diagnostics.into_iter() {
                        for file in ingot.files(db.as_input_db()) {
                            let file = *file;
                            let path = file.path(db.as_input_db());
                            let path = lsp_types::Url::from_file_path(path).unwrap();
                            let db = db.snapshot();
                            let client = client.clone();
                            let workspace = workspace.clone();
                            self.workers.spawn(
                                async move { handle_diagnostics(client.clone(), workspace.clone(), db, path).await }
                            );
                        }
                    }
                }
                Some((params, responder)) = hover_stream.next() => {
                    let db = db.snapshot();
                    let workspace = workspace.clone();
                    let response = match self.workers.spawn(handle_hover(db, workspace, params)).await {
                        Ok(response) => response,
                        Err(e) => {
                            eprintln!("Error handling hover: {:?}", e);
                            Ok(None)
                        }
                    };
                    let _ = responder.send(response);
                }
                Some((params, responder)) = goto_definition_stream.next() => {
                    let db = db.snapshot();
                    let workspace = workspace.clone();
                    let response = match handle_goto_definition(db, workspace, params).await {
                        Ok(response) => response,
                        Err(e) => {
                            eprintln!("Error handling goto definition: {:?}", e);
                            None
                        }
                    };
                    let _ = responder.send(Ok(response));
                }
            }
            tokio::task::yield_now().await;
        }
    }
}

async fn update_input(
    workspace: Arc<RwLock<Workspace>>,
    db: &mut LanguageServerDatabase,
    path: &str,
    contents: Option<String>,
) {
    info!("updating input for {:?}", path);
    let workspace = &mut workspace.write().await;
    let input = workspace.touch_input_for_file_path(db, path).unwrap();
    if let Some(contents) = contents {
        let _ = input.sync_from_text(db, contents);
    }
}

async fn handle_diagnostics(
    client: Client,
    workspace: Arc<RwLock<Workspace>>,
    db: Snapshot<LanguageServerDatabase>,
    url: lsp_types::Url,
) {
    info!("handling diagnostics for {:?}", url);
    let workspace = &workspace.read().await;
    let diagnostics = get_diagnostics(&db, workspace, url.clone());

    let client = client.clone();
    let diagnostics = diagnostics
        .unwrap()
        .into_iter()
        .map(|(uri, diags)| async { client.publish_diagnostics(uri, diags, None).await })
        .collect::<Vec<_>>();

    futures::future::join_all(diagnostics).await;
}
