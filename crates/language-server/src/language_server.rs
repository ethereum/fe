use std::{sync::Arc};


use lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams, FileSystemWatcher, GlobPattern, InitializeParams, InitializeResult, Registration
};

use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

use crate::{
    capabilities::server_capabilities,
};

// This is replaced by the `dispatcher` procedural macro!
// struct Dispatch {
//     tx_initialize: tokio::sync::mpsc::Sender<InitializeParams>,
//     initialize_stream: tokio_stream::wrappers::ReceiverStream<InitializeParams>,
//     tx_did_open: tokio::sync::mpsc::Sender<TextDocumentItem>,
//     did_open_stream: tokio_stream::wrappers::ReceiverStream<TextDocumentItem>,
//     tx_did_change: tokio::sync::mpsc::Sender<TextDocumentItem>,
//     did_change_stream: tokio_stream::wrappers::ReceiverStream<TextDocumentItem>,
//     tx_did_close: tokio::sync::mpsc::Sender<DidCloseTextDocumentParams>,
//     did_close_stream: tokio_stream::wrappers::ReceiverStream<DidCloseTextDocumentParams>,
//     tx_did_change_watched_files: tokio::sync::mpsc::Sender<DidChangeWatchedFilesParams>,
//     did_change_watched_files_stream: tokio_stream::wrappers::ReceiverStream<DidChangeWatchedFilesParams>,
// }

// impl Dispatch {
//     fn new() -> Self {
//         let (tx_initialize, rx_initialize) = tokio::sync::mpsc::channel(16);
//         let (tx_did_open, rx_did_open) = tokio::sync::mpsc::channel(16);
//         let (tx_did_change, rx_did_change) = tokio::sync::mpsc::channel(16);
//         let (tx_did_close, rx_did_close) = tokio::sync::mpsc::channel(16);
//         let (tx_did_change_watched_files, rx_did_change_watched_files) = tokio::sync::mpsc::channel(16);
//         Self {
//             tx_initialize,
//             tx_did_open,
//             tx_did_change,
//             tx_did_close,
//             tx_did_change_watched_files,
//             initialize_stream: tokio_stream::wrappers::ReceiverStream::new(rx_initialize),
//             did_open_stream: tokio_stream::wrappers::ReceiverStream::new(rx_did_open),
//             did_close_stream: tokio_stream::wrappers::ReceiverStream::new(rx_did_close),
//             did_change_stream: tokio_stream::wrappers::ReceiverStream::new(rx_did_change),
//             did_change_watched_files_stream: tokio_stream::wrappers::ReceiverStream::new(rx_did_change_watched_files),
//         }
//     }
// }

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
        Ok(client.register_capability(vec![registration]).await?)
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

        let initialize_result = rx.await.unwrap();
        initialize_result
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        let _ = self.dispatch.did_open_tx.send(params);
        // self.tx.send(Box::new(params));
        // info!("did open: {:?}", params);
        // {
        //     let workspace = &mut self.workspace.lock().unwrap();
        //     let db = &mut self.db.lock().unwrap();
        //     let _ = workspace.sync(db);
        // }

        // on_change(
        //     self,
        //     TextDocumentItem {
        //         uri: params.text_document.uri,
        //         language_id: LANGUAGE_ID.to_string(),
        //         version: params.text_document.version,
        //         text: params.text_document.text,
        //     },
        // )
        // .await;
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        let _ = self.dispatch.did_change_tx.send(params);
        // info!("did change: {:?}", params);
        // on_change(
        //     self,
        //     TextDocumentItem {
        //         uri: params.text_document.uri,
        //         language_id: LANGUAGE_ID.to_string(),
        //         version: params.text_document.version,
        //         text: params.content_changes[0].text.clone(),
        //     },
        // )
        // .await;
    }

    // Currently this is used to handle document renaming since the "document open" handler is called
    // before the "document was renamed" handler.
    //
    // The fix: handle document renaming more explicitly in the "will rename" flow, along with the document
    // rename refactor.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let _ = self.dispatch.did_close_tx.send(params);
        // let workspace = &mut self.workspace.lock().unwrap();
        // // let workspace = &mut workspace.lock().await;
        // let db = &mut self.db.lock().unwrap();
        // // let db = &mut db.lock().await;

        // let input = workspace
        //     .input_from_file_path(
        //         db,
        //         params
        //             .text_document
        //             .uri
        //             .to_file_path()
        //             .unwrap()
        //             .to_str()
        //             .unwrap(),
        //     )
        //     .unwrap();
        // let _ = input.sync(db, None);
    }
    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let _ = self.dispatch.did_change_watched_files_tx.send(params);
        // let changes = params.changes;
        // for change in changes {
        //     let uri = change.uri;
        //     let path = uri.to_file_path().unwrap();

        //     match change.typ {
        //         lsp_types::FileChangeType::CREATED => {
        //             // TODO: handle this more carefully!
        //             // this is inefficient, a hack for now
        //             let workspace = &mut self.workspace.lock().unwrap();
        //             let db = &mut self.db.lock().unwrap();
        //             let _ = workspace.sync(db);
        //             let input = workspace
        //                 .input_from_file_path(db, path.to_str().unwrap())
        //                 .unwrap();
        //             let _ = input.sync(db, None);
        //         }
        //         lsp_types::FileChangeType::CHANGED => {
        //             let workspace = &mut self.workspace.lock().unwrap();
        //             let db = &mut self.db.lock().unwrap();
        //             let input = workspace
        //                 .input_from_file_path(db, path.to_str().unwrap())
        //                 .unwrap();
        //             let _ = input.sync(db, None);
        //         }
        //         lsp_types::FileChangeType::DELETED => {
        //             // TODO: handle this more carefully!
        //             // this is inefficient, a hack for now
        //             let workspace = &mut self.workspace.lock().unwrap();
        //             let db = &mut self.db.lock().unwrap();
        //             let _ = workspace.sync(db);
        //         }
        //         _ => {}
        //     }
        //     // collect diagnostics for the file
        //     if change.typ != lsp_types::FileChangeType::DELETED {
        //         let text = std::fs::read_to_string(path).unwrap();
        //         on_change(
        //             self,
        //             TextDocumentItem {
        //                 uri: uri.clone(),
        //                 language_id: LANGUAGE_ID.to_string(),
        //                 version: 0,
        //                 text,
        //             },
        //         )
        //         .await;
        //     }
        // }
    }
}

// async fn on_change(backend: &Backend, params: TextDocumentItem) {
//     let diagnostics = {
//         let workspace = &mut backend.workspace.lock().unwrap();
//         let db = &mut backend.db.lock().unwrap();
//         let input = workspace
//             .input_from_file_path(
//                 db,
//                 params
//                     .uri
//                     .to_file_path()
//                     .expect("Failed to convert URI to file path")
//                     .to_str()
//                     .expect("Failed to convert file path to string"),
//             )
//             .unwrap();
//         let _ = input.sync(db, Some(params.text));
//         get_diagnostics(db, workspace, params.uri.clone())
//     };

//     let client = backend.client.lock().await;
//     let diagnostics = diagnostics
//         .unwrap()
//         .into_iter()
//         .map(|(uri, diags)| client.publish_diagnostics(uri, diags, None))
//         .collect::<Vec<_>>();

//     futures::future::join_all(diagnostics).await;
// }
