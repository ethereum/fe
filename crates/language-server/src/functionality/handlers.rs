use crate::backend::Backend;

use async_lsp::lsp_types::FileChangeType;
use async_lsp::{
    lsp_types::{
        Hover, HoverParams, InitializeParams, InitializeResult, InitializedParams, LogMessageParams,
    },
    LanguageClient, ResponseError,
};
use common::InputDb;
use futures::TryFutureExt;
use fxhash::FxHashSet;
use salsa::ParallelDatabase;

use super::{capabilities::server_capabilities, hover::hover_helper};

use crate::backend::workspace::IngotFileContext;

use tracing::{error, info, instrument::WithSubscriber};

#[derive(Debug)]
pub struct FilesNeedDiagnostics(pub Vec<NeedsDiagnostics>);

#[derive(Debug)]
pub struct NeedsDiagnostics(pub url::Url);

impl std::fmt::Display for FilesNeedDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FilesNeedDiagnostics({:?})", self.0)
    }
}

impl std::fmt::Display for NeedsDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FileNeedsDiagnostics({})", self.0)
    }
}

#[derive(Debug)]
pub struct FileChange {
    pub uri: url::Url,
    pub kind: ChangeKind,
}

#[derive(Debug)]
pub enum ChangeKind {
    Open(String),
    Create,
    Edit(Option<String>),
    Delete,
}

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

    let root = message
        .workspace_folders
        .and_then(|folders| folders.first().cloned())
        .and_then(|folder| folder.uri.to_file_path().ok())
        .unwrap_or_else(|| std::env::current_dir().unwrap());

    let _ = backend.workspace.set_workspace_root(&mut backend.db, &root);
    let _ = backend.workspace.load_std_lib(&mut backend.db, &root);
    // let _ = backend.workspace.sync();

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
    backend: &Backend,
    _message: InitializedParams,
) -> Result<(), ResponseError> {
    info!("language server initialized! recieved notification!");

    backend.workspace.all_files().for_each(|file| {
        let path = file.path(backend.db.as_input_db());
        let _ = backend
            .client
            .emit(NeedsDiagnostics(url::Url::from_file_path(path).unwrap()));
    });

    let _ = backend.client.clone().log_message(LogMessageParams {
        typ: async_lsp::lsp_types::MessageType::INFO,
        message: "language server initialized!".to_string(),
    });
    Ok(())
}

pub async fn handle_exit(_backend: &Backend, _message: ()) -> Result<(), ResponseError> {
    info!("shutting down language server");
    Ok(())
}

pub async fn handle_did_change_watched_files(
    backend: &Backend,
    message: async_lsp::lsp_types::DidChangeWatchedFilesParams,
) -> Result<(), ResponseError> {
    for event in message.changes {
        let kind = match event.typ {
            FileChangeType::CHANGED => ChangeKind::Edit(None),
            FileChangeType::CREATED => ChangeKind::Create,
            FileChangeType::DELETED => ChangeKind::Delete,
            _ => unreachable!(),
        };
        let _ = backend.client.clone().emit(FileChange {
            uri: event.uri,
            kind,
        });
    }
    Ok(())
}

pub async fn handle_did_open_text_document(
    backend: &Backend,
    message: async_lsp::lsp_types::DidOpenTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file opened: {:?}", message.text_document.uri);
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Open(message.text_document.text),
    });
    Ok(())
}

pub async fn handle_did_change_text_document(
    backend: &Backend,
    message: async_lsp::lsp_types::DidChangeTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file changed: {:?}", message.text_document.uri);
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Edit(Some(message.content_changes[0].text.clone())),
    });
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

    let _ = backend.client.emit(NeedsDiagnostics(message.uri));
    Ok(())
}

pub async fn handle_files_need_diagnostics(
    backend: &mut Backend,
    message: FilesNeedDiagnostics,
) -> Result<(), ResponseError> {
    let FilesNeedDiagnostics(need_diagnostics) = message;
    let client = backend.client.clone();
    let ingot_files_need_diagnostics: FxHashSet<_> = need_diagnostics
        .into_iter()
        .filter_map(|NeedsDiagnostics(file)| backend.workspace.get_ingot_for_file_path(file.path()))
        .flat_map(|ingot| ingot.files(backend.db.as_input_db()))
        .cloned()
        .collect();

    let uris: Vec<url::Url> = ingot_files_need_diagnostics
        .iter()
        .map(|input_file| {
            let path_str = input_file.path(backend.db.as_input_db());
            url::Url::from_file_path(path_str).unwrap()
        })
        .collect();

    info!("computing diagnostics for files: {:?}", uris);

    let db = backend.db.snapshot();
    let compute_and_send_diagnostics = backend
        .workers
        .spawn_blocking(move || {
            // Get diagnostics per file
            let diagnostics_map =
                db.get_lsp_diagnostics(ingot_files_need_diagnostics.iter().cloned().collect());
            // Return the diagnostics map and the pre-collected URIs
            diagnostics_map
        })
        .and_then(move |diagnostics_map| async move {
            // For each URI, get its diagnostics or empty
            let mut client = client.clone();
            for uri in uris {
                let diagnostic = diagnostics_map.get(&uri).cloned().unwrap_or_default();
                let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: diagnostic,
                    version: None,
                };
                client.publish_diagnostics(diagnostics_params).unwrap();
            }

            Ok(())
        });
    backend
        .workers
        .spawn(compute_and_send_diagnostics.with_current_subscriber());
    Ok(())
}

pub async fn handle_hover_request(
    backend: &Backend,
    message: HoverParams,
) -> Result<Option<Hover>, ResponseError> {
    let file = backend.workspace.get_input_for_file_path(
        message
            .text_document_position_params
            .text_document
            .uri
            .path(),
    );
    info!("handling hover request in file: {:?}", file);

    let response = file.and_then(|file| {
        hover_helper(&backend.db, file, message).unwrap_or_else(|e| {
            error!("Error handling hover: {:?}", e);
            None
        })
    });

    info!("sending hover response: {:?}", response);
    Ok(response)
}
