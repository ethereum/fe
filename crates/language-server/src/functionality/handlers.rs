use crate::backend::Backend;

use crate::backend::workspace::SyncableIngotFileContext;

use async_lsp::{
    lsp_types::{
        notification::Exit, Hover, HoverParams, InitializeParams, InitializeResult,
        InitializedParams,
    }, LanguageClient, ResponseError,
};
use common::InputDb;
use futures::TryFutureExt;
use fxhash::FxHashSet;
use salsa::ParallelDatabase;

use super::{
    capabilities::server_capabilities,
    hover::hover_helper,
    streams::{ChangeKind, FileChange},
};

// use crate::lsp_actor::*;

use crate::backend::workspace::IngotFileContext;

use tracing::{error, info};

pub type FilesNeedDiagnostics = Vec<String>;
// impl Backend {

impl Backend {
    fn update_input_file_text(&mut self, path: &str, contents: String) {
        let input = self
            .workspace
            .touch_input_for_file_path(&mut self.db, path)
            .unwrap();
        input.set_text(&mut self.db).to(contents);
    }
}

pub async fn initialize(
    backend: &mut Backend,
    message: InitializeParams,
) -> Result<InitializeResult, ResponseError> {
    info!("initializing language server!");

    let root = message.root_uri.unwrap().to_file_path().ok().unwrap();

    // disabled for now
    let _ = backend.workspace.set_workspace_root(&mut backend.db, &root);
    let _ = backend.workspace.load_std_lib(&mut backend.db, &root);
    let _ = backend.workspace.sync(&mut backend.db);

    let capabilities = server_capabilities();
    let initialize_result = InitializeResult {
        capabilities,
        server_info: Some(async_lsp::lsp_types::ServerInfo {
            name: String::from("fe-language-server"),
            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
        }),
    };
    Ok(initialize_result)
}

pub async fn initialized(
    _backend: &mut Backend,
    _message: InitializedParams,
) -> Result<(), ResponseError> {
    info!("language server initialized! recieved notification!");
    Ok(())
}

pub async fn handle_exit(_backend: &mut Backend, _message: Exit) -> Result<(), ResponseError> {
    info!("shutting down language server");
    Ok(())
}

pub async fn handle_file_change(
    backend: &mut Backend,
    message: FileChange,
) -> Result<(), ResponseError> {
    let path = message
        .uri
        .to_file_path()
        .unwrap_or_else(|_| panic!("Failed to convert URI to path: {:?}", message.uri));

    let path = path.to_str().unwrap();

    match message.kind {
        ChangeKind::Open(contents) => {
            info!("file opened: {:?}", &path);
            backend.update_input_file_text(path, contents);
        }
        ChangeKind::Create => {
            info!("file created: {:?}", &path);
            let contents = tokio::fs::read_to_string(&path).await.unwrap();
            backend.update_input_file_text(path, contents)
        }
        ChangeKind::Edit(contents) => {
            info!("file edited: {:?}", &path);
            let contents = if let Some(text) = contents {
                text
            } else {
                tokio::fs::read_to_string(&path).await.unwrap()
            };
            backend.update_input_file_text(path, contents);
        }
        ChangeKind::Delete => {
            info!("file deleted: {:?}", path);
            backend
                .workspace
                .remove_input_for_file_path(&mut backend.db, path)
                .unwrap();
        }
    }
    Ok(())
}

pub async fn handle_files_need_diagnostics(
    backend: &mut Backend,
    message: FilesNeedDiagnostics,
) -> Result<(), ResponseError> {
    let client = backend.client.clone();
    let ingot_files_need_diagnostics: FxHashSet<_> = message
        .into_iter()
        .filter_map(|file| backend.workspace.get_ingot_for_file_path(&file))
        .flat_map(|ingot| ingot.files(backend.db.as_input_db()))
        .cloned()
        .collect();

    let db = backend.db.snapshot();
    let compute_and_send_diagnostics = backend
        .workers
        .spawn_blocking(move || {
            db.get_lsp_diagnostics(ingot_files_need_diagnostics.into_iter().collect())
        })
        .and_then(|diagnostics| async move {
            futures::future::join_all(diagnostics.into_iter().map(|(path, diagnostic)| {
                let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
                    uri: path,
                    diagnostics: diagnostic,
                    version: None,
                };
                let mut client = client.clone();
                async move { client.publish_diagnostics(diagnostics_params) }
            }))
            .await;
            Ok(())
        });
    tokio::spawn(compute_and_send_diagnostics);
    Ok(())
}

pub async fn handle_hover_request(
    backend: &mut Backend,
    message: HoverParams,
) -> Result<Option<Hover>, ResponseError> {
    let file = backend.workspace.get_input_for_file_path(
        message
            .text_document_position_params
            .text_document
            .uri
            .path(),
    );

    let response = file.and_then(|file| {
        hover_helper(&backend.db, file, message).unwrap_or_else(|e| {
            error!("Error handling hover: {:?}", e);
            None
        })
    });

    Ok(response)
}
