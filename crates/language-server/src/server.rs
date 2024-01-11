use lsp_types::{HoverProviderCapability, ServerCapabilities};

#[cfg(target_arch = "wasm32")]
use crate::util::DummyFilePathConversion;

pub(crate) fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // full sync mode for now
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::FULL,
        )),
        // goto definition
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        // support for workspace add/remove changes
        workspace: Some(lsp_types::WorkspaceServerCapabilities {
            workspace_folders: Some(lsp_types::WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(lsp_types::OneOf::Left(true)),
            }),
            file_operations: Some(lsp_types::WorkspaceFileOperationsServerCapabilities {
                did_create: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some(String::from("file")),
                        pattern: lsp_types::FileOperationPattern {
                            glob: String::from("**/*"),
                            options: None,
                            // options: Some(lsp_types::FileOperationPatternOptions {
                            //     ignore_case: Some(true),
                            // }),
                            matches: None,
                        },
                    }],
                }),
                did_rename: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some(String::from("file")),
                        pattern: lsp_types::FileOperationPattern {
                            glob: String::from("**/*"),
                            options: None,
                            // options: Some(lsp_types::FileOperationPatternOptions {
                            //     ignore_case: Some(true),
                            // }),
                            matches: None,
                        },
                    }],
                }),
                did_delete: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some(String::from("file")),
                        pattern: lsp_types::FileOperationPattern {
                            glob: String::from("**/*"),
                            options: None,
                            // options: Some(lsp_types::FileOperationPatternOptions {
                            //     ignore_case: Some(true),
                            // }),
                            matches: None,
                        },
                    }],
                }),
                will_create: None,
                will_rename: None,
                will_delete: None,
                // TODO: implement file operation refactors and workspace cache updates
                // will_create: Some(lsp_types::FileOperationRegistrationOptions {
                //     filters: vec![lsp_types::FileOperationFilter {
                //         scheme: Some(String::from("file")),
                //         pattern: lsp_types::FileOperationPattern {
                //             glob: String::from("**/*"),
                //             options: None,
                //             matches: None,
                //         },
                //     }],
                // }),
                // will_rename: Some(lsp_types::FileOperationRegistrationOptions {
                //     filters: vec![lsp_types::FileOperationFilter {
                //         scheme: Some(String::from("file")),
                //         pattern: lsp_types::FileOperationPattern {
                //             glob: String::from("**/*"),
                //             options: None,
                //             matches: None,
                //         },
                //     }],
                // }),
                // will_delete: Some(lsp_types::FileOperationRegistrationOptions {
                //     filters: vec![lsp_types::FileOperationFilter {
                //         scheme: Some(String::from("file")),
                //         pattern: lsp_types::FileOperationPattern {
                //             glob: String::from("**/*"),
                //             options: None,
                //             matches: None,
                //         },
                //     }],
                // }),
            }),
        }),
        // ..Default::default()
        ..Default::default()
    }
}

// pub fn run_server() -> Result<()> {
//     let (connection, io_threads) = Connection::stdio();

//     let (request_id, _initialize_params) = connection.initialize_start()?;
//     let initialize_params: InitializeParams = serde_json::from_value(_initialize_params)?;

//     let capabilities = server_capabilities();

//     let initialize_result = lsp_types::InitializeResult {
//         capabilities,
//         server_info: Some(lsp_types::ServerInfo {
//             name: String::from("fe-language-server"),
//             version: Some(String::from(env!("CARGO_PKG_VERSION"))),
//         }),
//     };

//     let initialize_result = serde_json::to_value(initialize_result).unwrap();

//     connection.initialize_finish(request_id, initialize_result)?;
//     // send a "hello" message to the client
//     connection
//         .sender
//         .send(lsp_server::Message::Notification(Notification {
//             method: String::from("window/showMessage"),
//             params: serde_json::to_value(lsp_types::ShowMessageParams {
//                 typ: lsp_types::MessageType::INFO,
//                 message: String::from("hello from the Fe language server"),
//             })
//             .unwrap(),
//         }))?;

//     // let client = Backend::new(connection.sender.clone());
//     let mut state = ServerState::new(connection.sender);
//     let _ = state.init_logger(log::Level::Info);
//     state.workspace.set_workspace_root(
//         &mut state.db,
//         initialize_params
//             .root_uri
//             .unwrap()
//             .to_file_path()
//             .ok()
//             .unwrap(),
//     )?;
//     // info!("TESTING");
//     // info!("initialized with params: {:?}", debug_params);

//     let result = state.run(connection.receiver);

//     io_threads.join().unwrap();

//     result
// }
