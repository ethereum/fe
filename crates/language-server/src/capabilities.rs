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
        ..Default::default()
    }
}