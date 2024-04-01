use crate::backend::Backend;

use crate::backend::workspace::SyncableIngotFileContext;

use common::InputDb;
use futures::TryFutureExt;
use fxhash::FxHashSet;

use lsp_types::TextDocumentItem;
use salsa::ParallelDatabase;

use super::{capabilities::server_capabilities, hover::hover_helper};

use crate::backend::workspace::IngotFileContext;

use tracing::{error, info};

impl Backend {
    pub(super) async fn handle_initialized(
        &mut self,
        params: lsp_types::InitializeParams,
        responder: tokio::sync::oneshot::Sender<
            Result<lsp_types::InitializeResult, tower_lsp::jsonrpc::Error>,
        >,
    ) {
        info!("initializing language server!");

        let root = params.root_uri.unwrap().to_file_path().ok().unwrap();

        let _ = self.workspace.set_workspace_root(&mut self.db, &root);
        let _ = self.workspace.load_std_lib(&mut self.db, &root);
        let _ = self.workspace.sync(&mut self.db);

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

    pub(super) async fn handle_shutdown(
        &mut self,
        responder: tokio::sync::oneshot::Sender<Result<(), tower_lsp::jsonrpc::Error>>,
    ) {
        info!("shutting down language server");
        let _ = responder.send(Ok(()));
    }

    pub(super) async fn handle_deleted(
        &mut self,
        params: lsp_types::FileEvent,
        tx_needs_diagnostics: tokio::sync::mpsc::UnboundedSender<String>,
    ) {
        let path = params.uri.to_file_path().unwrap();
        info!("file deleted: {:?}", path);
        let path = path.to_str().unwrap();
        let _ = self
            .workspace
            .remove_input_for_file_path(&mut self.db, path);
        let _ = tx_needs_diagnostics.send(path.to_string());
    }

    pub(super) async fn handle_change(
        &mut self,
        doc: TextDocumentItem,
        tx_needs_diagnostics: tokio::sync::mpsc::UnboundedSender<String>,
    ) {
        info!("change detected: {:?}", doc.uri);
        let path_buf = doc.uri.to_file_path().unwrap();
        let path = path_buf.to_str().unwrap();
        let contents = Some(doc.text);
        if let Some(contents) = contents {
            let input = self
                .workspace
                .touch_input_for_file_path(&mut self.db, path)
                .unwrap();
            input.set_text(&mut self.db).to(contents);
        }
        let _ = tx_needs_diagnostics.send(path.to_string());
    }

    pub(super) async fn handle_diagnostics(&mut self, files_need_diagnostics: Vec<String>) {
        let ingot_files_need_diagnostics: FxHashSet<_> = files_need_diagnostics
            .into_iter()
            .filter_map(|file| self.workspace.get_ingot_for_file_path(&file))
            .flat_map(|ingot| ingot.files(self.db.as_input_db()))
            .cloned()
            .collect();

        let db = self.db.snapshot();
        let client = self.client.clone();
        let compute_and_send_diagnostics = self
            .workers
            .spawn_blocking(move || {
                db.get_lsp_diagnostics(ingot_files_need_diagnostics.into_iter().collect())
            })
            .and_then(|diagnostics| async move {
                futures::future::join_all(diagnostics.into_iter().map(|(path, diagnostic)| {
                    let client = client.clone();
                    async move { client.publish_diagnostics(path, diagnostic, None).await }
                }))
                .await;
                Ok(())
            });
        tokio::spawn(compute_and_send_diagnostics);
    }

    pub(super) async fn handle_hover(
        &mut self,
        params: lsp_types::HoverParams,
        responder: tokio::sync::oneshot::Sender<
            Result<Option<lsp_types::Hover>, tower_lsp::jsonrpc::Error>,
        >,
    ) {
        // let db = self.db.snapshot();
        let file = self.workspace.get_input_for_file_path(
            params
                .text_document_position_params
                .text_document
                .uri
                .path(),
        );

        let response = file.and_then(|file| {
            hover_helper(&self.db, file, params).unwrap_or_else(|e| {
                error!("Error handling hover: {:?}", e);
                None
            })
        });

        let _ = responder.send(Ok(response));
    }
}
