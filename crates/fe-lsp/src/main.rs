use tower_lsp::jsonrpc::Result as LSPResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

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

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
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
    // better name???
    fn analysis_document(text_document: TextDocumentItem) -> Vec<Diagnostic> {
        let mut db = fe_driver::Db::default();

        // check project
        let diags = fe_driver::check_single_file(
            &mut db,
            &text_document.uri.to_string(),
            &text_document.text.to_string(),
        );

        return diags
            .into_iter()
            .map(|diag| diag.into_lsp(&db))
            .collect::<Vec<_>>();
    }

    async fn on_change(&self, text_document: TextDocumentItem) {
        self.client
            .log_message(MessageType::INFO, format!("{:?}", text_document))
            .await;

        let diags = Backend::analysis_document(text_document.clone());

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
