use codespan_lsp::byte_span_to_range;
use tower_lsp::jsonrpc::Result as LSPResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use std::fs::File;
use std::path::Path;

use fe_common::diagnostics::{self, print_diagnostics, Diagnostic as FeDiagnostic, Severity};
use fe_driver::Db;

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[derive(Debug, Clone)]
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}
#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, initialize_params: InitializeParams) -> LSPResult<InitializeResult> {
        self.client
            .log_message(
                MessageType::INFO,
                format!("{:#?}", initialize_params.capabilities),
            )
            .await;
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
        self.client
            .log_message(MessageType::INFO, "log form me!")
            .await;
    }

    async fn shutdown(&self) -> LSPResult<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> LSPResult<Option<CompletionResponse>> {
        self.client
            .log_message(MessageType::INFO, "log form complete!")
            .await;
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: HoverParams) -> LSPResult<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did_change event!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }
}

impl Backend {
    fn check(text_document: TextDocumentItem) -> Vec<Diagnostic> {
        let mut db = fe_driver::Db::default();

        // check project
        let diags = fe_driver::check_single_file(
            &mut db,
            &text_document.uri.to_string(),
            &text_document.text.to_string(),
        );

        return diags
            .into_iter()
            .map(|diag| Diagnostic {
                range: Backend::get_range(&db, diag.clone()),
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: None,
                message: diag.message,
                related_information: None,
                tags: None,
                data: None,
            })
            .collect::<Vec<_>>();
    }

    fn get_range(db: &Db, diag: FeDiagnostic) -> lsp_types::Range {
        let tmp = byte_span_to_range(
            &SourceDbWrapperLSP(db),
            diag.labels[0].span.file_id,
            std::ops::Range {
                start: diag.labels[0].span.start,
                end: diag.labels[0].span.end,
            },
        )
        .unwrap();
        return lsp_types::Range {
            start: Position {
                line: tmp.start.line,
                character: tmp.start.character,
            },
            end: Position {
                line: tmp.end.line,
                character: tmp.end.character,
            },
        };
    }

    async fn on_change(&self, text_document: TextDocumentItem) {
        self.client
            .log_message(MessageType::INFO, format!("{:?}", text_document))
            .await;

        let diags = Backend::check(text_document.clone());

        self.client
            .publish_diagnostics(
                text_document.uri.clone(),
                diags,
                Some(text_document.version),
            )
            .await;
    }
}
#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}

use codespan_reporting::files::Error as CsError;
use fe_common::db::SourceDb;
use fe_common::files::{SourceFileId, Utf8PathBuf};
use std::ops::Range;
use std::rc::Rc;
struct SourceDbWrapperLSP<'a>(pub &'a dyn SourceDb);

impl<'a> codespan_reporting::files::Files<'_> for SourceDbWrapperLSP<'a> {
    type FileId = SourceFileId;
    type Name = Rc<Utf8PathBuf>;
    type Source = Rc<str>;

    fn name(&self, file: SourceFileId) -> Result<Self::Name, CsError> {
        Ok(file.path(self.0))
    }

    fn source(&self, file: SourceFileId) -> Result<Self::Source, CsError> {
        Ok(file.content(self.0))
    }

    fn line_index(&self, file: SourceFileId, byte_index: usize) -> Result<usize, CsError> {
        Ok(file.line_index(self.0, byte_index))
    }

    fn line_range(&self, file: SourceFileId, line_index: usize) -> Result<Range<usize>, CsError> {
        file.line_range(self.0, line_index)
            .ok_or(CsError::LineTooLarge {
                given: line_index,
                max: self.0.file_line_starts(file).len() - 1,
            })
    }
}
