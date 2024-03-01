use crate::workspace::SyncableIngotFileContext;
use futures::TryStreamExt;
use lsp_types::TextDocumentItem;
use std::sync::Arc;
use tokio::sync::Mutex;

// use tokio::sync::oneshot::Receiver;

use crate::capabilities::server_capabilities;
use crate::db::LanguageServerDatabase;

use crate::diagnostics::get_diagnostics;
use crate::globals::LANGUAGE_ID;
use crate::language_server::Server;
use crate::workspace::{IngotFileContext, SyncableInputFile, Workspace};

use log::info;

use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::StreamExt;
use tower_lsp::Client;

pub struct Backend<'a> {
    pub(crate) server: &'a Server,
    pub(crate) client: Arc<tokio::sync::Mutex<Client>>,
    pub(crate) db: Arc<Mutex<LanguageServerDatabase>>,
    pub(crate) workspace: Arc<Mutex<Workspace>>,
    runtime: tokio::runtime::Runtime,
}

impl<'a> Backend<'a> {
    pub fn new(client: Arc<tokio::sync::Mutex<Client>>, server: &'a Server) -> Self {
        let workspace = Arc::new(Mutex::new(Workspace::default()));
        let db = Arc::new(Mutex::new(LanguageServerDatabase::default()));
        let runtime = tokio::runtime::Runtime::new().unwrap();

        Self {
            server,
            client,
            db,
            workspace,
            runtime,
        }
    }
    pub fn setup_streams(self) {
        let db = self.db.clone();
        let workspace = self.workspace.clone();
        let client = self.client.clone();
        let mut initialized_stream =
            BroadcastStream::new(self.server.dispatch.subscribe_initialize());
        self.runtime.spawn(async move {
            while let Some(result) = initialized_stream.next().await {
                if let Ok((initialization_params, responder)) = result {
                    info!("initializing language server: {:?}", initialization_params);
                    // setup workspace
                    let capabilities = server_capabilities();
                    let initialize_result = lsp_types::InitializeResult {
                        capabilities,
                        server_info: Some(lsp_types::ServerInfo {
                            name: String::from("fe-language-server"),
                            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
                        }),
                    };
                    let db = &mut self.db.lock().await;
                    let workspace = &mut self.workspace.lock().await;
                    let _ = workspace.set_workspace_root(
                        db,
                        initialization_params
                            .root_uri
                            .unwrap()
                            .to_file_path()
                            .ok()
                            .unwrap(),
                    );

                    responder.respond(Ok(initialize_result));
                }
            }
        });

        let mut shutdown_stream = BroadcastStream::new(self.server.dispatch.subscribe_shutdown());
        self.runtime.spawn(async move {
            while let Some(result) = shutdown_stream.next().await {
                if let Ok((_, responder)) = result {
                    info!("shutting down language server");
                    responder.respond(Ok(()));
                }
            }
        });

        let did_open_stream = BroadcastStream::new(self.server.dispatch.subscribe_did_open());
        let did_change_stream = BroadcastStream::new(self.server.dispatch.subscribe_did_change());

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
        );

        let workspace_clone = workspace.clone();
        let client_clone = client.clone();
        let db_clone = db.clone();
        self.runtime.spawn(async move {
            let workspace = &mut workspace_clone.lock().await;
            let client = &mut client_clone.lock().await;
            let db = &mut db_clone.lock().await;
            while let Some(Ok(doc)) = change_stream.next().await {
                on_change(client, workspace, db, doc).await;
            }
        });

        let workspace_clone = workspace.clone();
        let client_clone = client.clone();
        let db_clone = db.clone();
        let mut did_close_stream = BroadcastStream::new(self.server.dispatch.subscribe_did_close());
        self.runtime.spawn(async move {
            let workspace = &mut workspace_clone.lock().await;
            let _client = &mut client_clone.lock().await;
            let db = &mut db_clone.lock().await;
            while let Some(Ok(params)) = did_close_stream.next().await {
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
        });

        let workspace_clone = workspace.clone();
        let client_clone = client.clone();
        let db_clone = db.clone();
        let mut did_change_watched_files_stream =
            BroadcastStream::new(self.server.dispatch.subscribe_did_change_watched_files());
        self.runtime.spawn(async move {
            let workspace = &mut workspace_clone.lock().await;
            let client = &mut client_clone.lock().await;
            let db = &mut db_clone.lock().await;

            while let Some(Ok(params)) = did_change_watched_files_stream.next().await {
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
                            client,
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
        });
    }
}

async fn on_change(
    client: &mut Client,
    workspace: &mut Workspace,
    db: &mut LanguageServerDatabase,
    params: TextDocumentItem,
) {
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
