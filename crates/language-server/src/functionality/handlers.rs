use crate::backend::Backend;

use crate::backend::workspace::SyncableIngotFileContext;

use async_lsp::{
    lsp_types::{InitializeParams, InitializeResult, InitializedParams},
    ResponseError,
};

use super::capabilities::server_capabilities;

// use crate::lsp_actor::*;

use crate::backend::workspace::IngotFileContext;

use tracing::info;

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
    // let _ = backend.workspace.set_workspace_root(&mut backend.db, &root);
    // let _ = backend.workspace.load_std_lib(&mut backend.db, &root);
    // let _ = backend.workspace.sync(&mut backend.db);

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

// // #[async_trait::async_trait(?Send)]
// impl RequestHandler<InitializeParams, LspResult<Initialize>> for Backend {
//     async fn handle(
//         &mut self,
//         message: InitializeParams,
//     ) -> Result<InitializeResult, ResponseError> {
//         info!("initializing language server!");

//         let root = message.root_uri.unwrap().to_file_path().ok().unwrap();

//         let _ = self.workspace.set_workspace_root(&mut self.db, &root);
//         let _ = self.workspace.load_std_lib(&mut self.db, &root);
//         let _ = self.workspace.sync(&mut self.db);

//         let capabilities = server_capabilities();
//         let initialize_result = InitializeResult {
//             capabilities,
//             server_info: Some(async_lsp::lsp_types::ServerInfo {
//                 name: String::from("fe-language-server"),
//                 version: Some(String::from(env!("CARGO_PKG_VERSION"))),
//             }),
//         };
//         Ok(initialize_result)
//     }
// }

// // #[async_trait::async_trait(?Send)]
// impl MessageHandler<Exit> for Backend {
//     async fn handle(&mut self, _message: ()) {
//         info!("shutting down language server");
//     }
// }

// impl Message for FileChange {
//     type Contents = FileChange;
// }

// // #[async_trait::async_trait(?Send)]
// impl MessageHandler<FileChange> for Backend {
//     async fn handle(&mut self, message: FileChange) {
//         let path = message
//             .uri
//             .to_file_path()
//             .unwrap_or_else(|_| panic!("Failed to convert URI to path: {:?}", message.uri));

//         let path = path.to_str().unwrap();

//         match message.kind {
//             ChangeKind::Open(contents) => {
//                 info!("file opened: {:?}", &path);
//                 self.update_input_file_text(path, contents);
//             }
//             ChangeKind::Create => {
//                 info!("file created: {:?}", &path);
//                 let contents = tokio::fs::read_to_string(&path).await.unwrap();
//                 self.update_input_file_text(path, contents)
//             }
//             ChangeKind::Edit(contents) => {
//                 info!("file edited: {:?}", &path);
//                 let contents = if let Some(text) = contents {
//                     text
//                 } else {
//                     tokio::fs::read_to_string(&path).await.unwrap()
//                 };
//                 self.update_input_file_text(path, contents);
//             }
//             ChangeKind::Delete => {
//                 info!("file deleted: {:?}", path);
//                 self.workspace
//                     .remove_input_for_file_path(&mut self.db, path)
//                     .unwrap();
//             }
//         }
//     }
// }

// impl Message for FilesNeedDiagnostics {
//     type Contents = FilesNeedDiagnostics;
// }
// impl MessageHandler<FilesNeedDiagnostics> for Backend {
//     async fn handle(&mut self, message: FilesNeedDiagnostics) {
//         let client = self.client.clone();
//         let ingot_files_need_diagnostics: FxHashSet<_> = message
//             .into_iter()
//             .filter_map(|file| self.workspace.get_ingot_for_file_path(&file))
//             .flat_map(|ingot| ingot.files(self.db.as_input_db()))
//             .cloned()
//             .collect();

//         let db = self.db.snapshot();
//         let compute_and_send_diagnostics = self
//             .workers
//             .spawn_blocking(move || {
//                 db.get_lsp_diagnostics(ingot_files_need_diagnostics.into_iter().collect())
//             })
//             .and_then(|diagnostics| async move {
//                 futures::future::join_all(diagnostics.into_iter().map(|(path, diagnostic)| {
//                     let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
//                         uri: path,
//                         diagnostics: diagnostic,
//                         version: None,
//                     };
//                     let mut client = client.clone();
//                     async move { client.publish_diagnostics(diagnostics_params) }
//                 }))
//                 .await;
//                 Ok(())
//             });
//         tokio::spawn(compute_and_send_diagnostics);
//     }
// }

// impl RequestHandler<HoverRequest> for Backend {
//     async fn handle(&mut self, message: HoverParams) -> Result<Option<Hover>, ResponseError> {
//         let file = self.workspace.get_input_for_file_path(
//             message
//                 .text_document_position_params
//                 .text_document
//                 .uri
//                 .path(),
//         );

//         let response = file.and_then(|file| {
//             hover_helper(&self.db, file, message).unwrap_or_else(|e| {
//                 error!("Error handling hover: {:?}", e);
//                 None
//             })
//         });

//         Ok(response)
//     }
// }
