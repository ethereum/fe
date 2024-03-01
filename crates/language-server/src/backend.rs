use crate::workspace::SyncableIngotFileContext;
use futures::stream::FuturesUnordered;
use futures::TryStreamExt;
use lsp_types::TextDocumentItem;
use std::sync::Arc;
use tokio::join;
use tokio::sync::Mutex;

// use tokio::sync::oneshot::Receiver;

use crate::capabilities::server_capabilities;
use crate::db::LanguageServerDatabase;

use crate::diagnostics::get_diagnostics;
use crate::globals::LANGUAGE_ID;
use crate::language_server::{LspChannels, Server};
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};

use log::info;

use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::StreamExt;
use tower_lsp::Client;

pub struct Backend {
    // pub(crate) server: Arc<Mutex<&'a Server>>,
    pub(crate) messaging: Arc<Mutex<LspChannels>>,
    pub(crate) client: Arc<Mutex<Client>>,
    pub(crate) db: Arc<Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Arc<Mutex<Workspace>>,
    // runtime: tokio::runtime::Runtime,
}

impl Backend {
    pub fn new(client: Arc<Mutex<Client>>, messaging: Arc<Mutex<LspChannels>>) -> Self {
        let workspace = Arc::new(Mutex::new(Workspace::default()));
        let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        // let runtime = tokio::runtime::Runtime::new().unwrap();

        Self {
            messaging,
            client,
            db,
            workspace,
            // runtime,
        }
    }
    pub async fn setup_streams(
        self,
        // messaging: &LspChannels, // , db: &LanguageServerDatabase, workspace: &Workspace, client: &Client
    ) {
        info!("setting up streams");
        info!("what's next");

        let db_wrapped = self.db.clone();
        let workspace_wrapped = self.workspace.clone();
        let client_wrapped = self.client.clone();
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

        // This is very important! We absolutely need to drop the messaging lock here.
        std::mem::drop(messaging);
        info!("streams set up, looping on them now");
        loop {
            tokio::select! {
                        Some(result) = initialized_stream.next() => {
            let db = &mut db_wrapped.lock().await;
            let workspace = &mut workspace_wrapped.lock().await;
            let client = &mut client_wrapped.lock().await;
                            info!("received initialize request {:?}", result);
                            if let Ok((initialization_params, responder)) = result {
                                info!("initializing language server: {:?}", initialization_params);
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

                                info!("initializing language server!");
                                // responder.respond(Ok(initialize_result));
                            }
                        }
                        // Some(result) = shutdown_stream.next() => {
                        //     if let Ok((_, responder)) = result {
                        //         info!("shutting down language server");
                        //         responder.respond(Ok(()));
                        //     }
                        // }
                        Some(Ok(doc)) = change_stream.next() => {
                            info!("change detected: {:?}", doc.uri);
                            on_change(client_wrapped.clone(), workspace_wrapped.clone(), db_wrapped.clone(), doc).await;
                        }
                        Some(Ok(params)) = did_close_stream.next() => {
            let db = &mut db_wrapped.lock().await;
            let workspace = &mut workspace_wrapped.lock().await;
            let client = &mut client_wrapped.lock().await;
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
                        Some(Ok(params)) = did_change_watched_files_stream.next() => {
            let db = &mut db_wrapped.lock().await;
            let workspace = &mut workspace_wrapped.lock().await;
            let client = &mut client_wrapped.lock().await;
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
                                        .input_from_file_path(db, path.to_str().unwrap())
                                        .unwrap();
                                    let _ = input.sync(db, None);
                                }
                                lsp_types::FileChangeType::CHANGED => {
                                    let input = workspace
                                        .input_from_file_path(db, path.to_str().unwrap())
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
                                    self.workspace.clone(),
                                    self.db.clone(),
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

                }

            //     while let Some(result) = initialized_stream.next().await {
            //         info!("received initialize request {:?}", result);
            //         if let Ok((initialization_params, responder)) = result {
            //             info!("initializing language server: {:?}", initialization_params);
            //             // setup workspace
            //             let db = &mut db.lock().await;
            //             let workspace = &mut workspace.lock().await;
            //             let _ = workspace.set_workspace_root(
            //                 db,
            //                 initialization_params
            //                     .root_uri
            //                     .unwrap()
            //                     .to_file_path()
            //                     .ok()
            //                     .unwrap(),
            //             );

            //             info!("initializing language server!");
            //             // responder.respond(Ok(initialize_result));
            //         }
            //     }
            // }

            // tokio::spawn(async move {
            //     while let Some(result) = shutdown_stream.next().await {
            //         if let Ok((_, responder)) = result {
            //             info!("shutting down language server");
            //             responder.respond(Ok(()));
            //         }
            //     }
            // });

            // async move {
            //     info!("listening for changes");
            //     while let Some(Ok(doc)) = change_stream.next().await {
            //         info!("change detected: {:?}", doc.uri);
            //         on_change(client.clone(), workspace.clone(), db.clone(), doc).await;
            //     }
            // }

            // async move {
            //     let workspace = &mut workspace_clone.lock().await;
            //     let _client = &mut client_clone.lock().await;
            //     let db = &mut db_clone.lock().await;
            //     while let Some(Ok(params)) = did_close_stream.next().await {
            //         let input = workspace
            //             .input_from_file_path(
            //                 db,
            //                 params
            //                     .text_document
            //                     .uri
            //                     .to_file_path()
            //                     .unwrap()
            //                     .to_str()
            //                     .unwrap(),
            //             )
            //             .unwrap();
            //         let _ = input.sync(db, None);
            //     }
            // }

            // async move {
            //     let workspace = &mut workspace_clone.lock().await;
            //     let client = &mut client_clone.lock().await;
            //     let db = &mut db_clone.lock().await;

            //     while let Some(Ok(params)) = did_change_watched_files_stream.next().await {
            //         let changes = params.changes;
            //         for change in changes {
            //             let uri = change.uri;
            //             let path = uri.to_file_path().unwrap();

            //             match change.typ {
            //                 lsp_types::FileChangeType::CREATED => {
            //                     // TODO: handle this more carefully!
            //                     // this is inefficient, a hack for now
            //                     let _ = workspace.sync(db);
            //                     let input = workspace
            //                         .input_from_file_path(db, path.to_str().unwrap())
            //                         .unwrap();
            //                     let _ = input.sync(db, None);
            //                 }
            //                 lsp_types::FileChangeType::CHANGED => {
            //                     let input = workspace
            //                         .input_from_file_path(db, path.to_str().unwrap())
            //                         .unwrap();
            //                     let _ = input.sync(db, None);
            //                 }
            //                 lsp_types::FileChangeType::DELETED => {
            //                     // TODO: handle this more carefully!
            //                     // this is inefficient, a hack for now
            //                     let _ = workspace.sync(db);
            //                 }
            //                 _ => {}
            //             }
            //             // collect diagnostics for the file
            //             if change.typ != lsp_types::FileChangeType::DELETED {
            //                 let text = std::fs::read_to_string(path).unwrap();
            //                 on_change(
            //                     self.client.clone(),
            //                     self.workspace.clone(),
            //                     self.db.clone(),
            //                     TextDocumentItem {
            //                         uri: uri.clone(),
            //                         language_id: LANGUAGE_ID.to_string(),
            //                         version: 0,
            //                         text,
            //                     },
            //                 )
            //                 .await;
            //             }
            //         }
            //     }
            // }
        }
    }
}

async fn on_change(
    client: Arc<Mutex<Client>>,
    workspace: Arc<Mutex<Workspace>>,
    db: Arc<Mutex<LanguageServerDatabase>>,
    params: TextDocumentItem,
) {
    let workspace = &mut workspace.lock().await;
    let db = &mut db.lock().await;
    let client = &mut client.lock().await;
    let diagnostics = {
        // let workspace = &mut workspace.lock().await;
        // let db = &mut db.lock().await;
        let input = workspace
            .input_from_file_path(
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

    // let client = client.lock().await;
    let diagnostics = diagnostics
        .unwrap()
        .into_iter()
        .map(|(uri, diags)| client.publish_diagnostics(uri, diags, None))
        .collect::<Vec<_>>();

    futures::future::join_all(diagnostics).await;
}
