use log::info;
use lsp_types::{
    DidChangeWatchedFilesParams, DidCloseTextDocumentParams, InitializeParams, InitializeResult,
    TextDocumentItem,
};

use tower_lsp::{jsonrpc::Result, LanguageServer};

use crate::{
    backend::Backend,
    capabilities::server_capabilities,
    diagnostics::get_diagnostics,
    globals::LANGUAGE_ID,
    workspace::{IngotFileContext, SyncableIngotFileContext, SyncableInputFile},
};

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, initialize_params: InitializeParams) -> Result<InitializeResult> {
        // initialize
        let capabilities = server_capabilities();
        let initialize_result = lsp_types::InitializeResult {
            capabilities,
            server_info: Some(lsp_types::ServerInfo {
                name: String::from("fe-language-server"),
                version: Some(String::from(env!("CARGO_PKG_VERSION"))),
            }),
        };
        // setup logging
        let _ = self.init_logger(log::Level::Info);

        // setup workspace
        let workspace = self.workspace();
        let workspace = &mut workspace.lock().await;
        let db = self.db();
        let db = &mut db.lock().await;

        let _ = workspace.set_workspace_root(
            db,
            initialize_params
                .root_uri
                .unwrap()
                .to_file_path()
                .ok()
                .unwrap(),
        );

        // register watchers
        let _ = self.register_watchers().await;

        Ok(initialize_result)
    }
    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        info!("did open: {:?}", params);
        let workspace = self.workspace();
        let workspace = &mut workspace.lock().await;
        let db = self.db();
        let db = &mut db.lock().await;
        let _ = workspace.sync(db);

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            language_id: LANGUAGE_ID.to_string(),
            version: params.text_document.version,
            text: params.text_document.text,
        })
        .await;
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        info!("did change: {:?}", params);
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            language_id: LANGUAGE_ID.to_string(),
            version: params.text_document.version,
            text: params.content_changes[0].text.clone(),
        })
        .await;
    }

    // Currently this is used to handle document renaming since the "document open" handler is called
    // before the "document was renamed" handler.
    //
    // The fix: handle document renaming more explicitly in the "will rename" flow, along with the document
    // rename refactor.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let workspace = self.workspace();
        let workspace = &mut workspace.lock().await;
        let db = self.db();
        let db = &mut db.lock().await;

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
    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let workspace = self.workspace();
        let workspace = &mut workspace.lock().await;
        let db = self.db();
        let db = &mut db.lock().await;

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
                // let diags = get_diagnostics(db, workspace, uri.clone());
                // for (uri, more_diags) in diags.ok().unwrap() {
                //     let diags = diagnostics.entry(uri).or_insert_with(Vec::new);
                //     diags.extend(more_diags);
                // }
                let text = std::fs::read_to_string(path).unwrap();
                self.on_change(TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text,
                })
                .await;
            }
        }
    }

    // async fn will_rename_files(&self, params: RenameFilesParams) -> Result<Option<WorkspaceEdit>> {
    //     let workspace = &mut *self.workspace.lock().await;
    //     let db = &mut *self.db.lock().await;

    //     for file in params.files {
    //         let _ = workspace.rename_file(db, &*file.old_uri, &*file.new_uri);
    //     }

    //     // TODO: implement file rename auto-refactoring
    //     Ok(None)
    // }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let workspace = self.workspace();
        let workspace = &mut workspace.lock().await;
        let db = self.db();
        let db = &mut db.lock().await;
        let client = self.client();
        let client = &mut *client.lock().await;
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
        let diagnostics = get_diagnostics(db, workspace, params.uri.clone())
            .unwrap()
            .into_iter()
            .map(|(uri, diags)| client.publish_diagnostics(uri, diags, None))
            .collect::<Vec<_>>();

        futures::future::join_all(diagnostics).await;
    }
}
