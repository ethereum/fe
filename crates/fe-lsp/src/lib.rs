use std::rc::Rc;
use std::string;

use codespan_lsp::byte_span_to_range;
use dashmap::DashMap;
use fe_common::SourceFileId;
use fe_common::db::SourceDb;
use fe_common::files::Utf8PathBuf;
use fe_driver::Db;
use fe_parser::ast::{FuncStmt, Function, Module, ModuleStmt, ContractStmt};
use fe_parser::node::Node;
use ropey::Rope;
use tower_lsp::jsonrpc::Result as LSPResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use codespan_reporting::files::{Error as CsError, Files};

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, Module>,
    content: DashMap<String, String>
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LSPResult<Option<GotoDefinitionResponse>> {
        let uri = params
        .text_document_position_params
        .text_document
        .uri;

        let file_id = uri
            .to_string();

        self.client
            .log_message(
                MessageType::INFO,
                format!("{}", self.content.get(&file_id).unwrap().to_string()),
            )
            .await;

        self.client
            .log_message(MessageType::INFO, format!("goto defined"))
            .await;

        let ast = self.ast_map.get(&file_id).unwrap();
        // self.client.log_message(MessageType::INFO, format!("{:#?}", ast)).await;
        let source = self.content.get(&file_id).unwrap().clone();

        let position = params.text_document_position_params.position;

        let char = Rope::from(source.clone())
            .try_line_to_char(position.line as usize)
            .unwrap();

        let offset = char + position.character as usize;

        // find sematic of current token


        self.client
        .log_message(MessageType::INFO, format!("input {}", &source[205..210]))
        .await;
        for stmt in &ast.body {
            if let ModuleStmt::Contract(contract) = stmt {
                for csmt in &contract.kind.body {
                    if let ContractStmt::Function(func) = csmt {
                    self.client.log_message(MessageType::INFO, format!("{:?}", func.kind.sig.kind.name.kind)).await;
                        if func.kind.sig.kind.name.kind == source[205..210] {
                            let sp = func.kind.sig.kind.name.span;
                            let f = file_id.clone();
                            let s = source.clone();
                            let span_file_id = FileId::new(&f, &s);

                            self.client.log_message(MessageType::INFO, format!("{:?}", sp)).await;
                            self.client.log_message(MessageType::INFO, format!("hehe {:?}", span_file_id.line_index(span_file_id, sp.start))).await;
                            self.client.log_message(MessageType::INFO, format!("hehe {:?}", span_file_id.line_range(span_file_id, 9))).await;
                            
                            let position = byte_span_to_range(&span_file_id, span_file_id, 
                                std::ops::Range {
                                    start: sp.start,
                                    end: sp.end
                                }
                            ).unwrap();

                            self.client.log_message(MessageType::INFO, format!("{:?}", position)).await;

                            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(uri, position))));
                        } 
                    }
                }
            }

            if let ModuleStmt::Function(func) = stmt {
                if source[func.span.start..func.span.end].to_string() == source[205..210] {
                    self.client
                        .log_message(MessageType::INFO, format!("found {}", &source[205..210]))
                        .await
                }
            }
        }
        self.client
            .log_message(MessageType::INFO, format!("Offset {}", offset))
            .await;
 
        Ok(None)
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
            .map(|diag| diag.into_lsps(&db))
            .flatten()
            .collect::<Vec<_>>();
    }

    async fn on_change(&self, text_document: TextDocumentItem) {
        let diags = Backend::analysis_document(text_document.clone());

        self.client
            .publish_diagnostics(
                text_document.uri.clone(),
                diags,
                Some(text_document.version),
            )
            .await;
        self.content
            .insert(text_document.uri.to_string(), text_document.text.clone());

        self.update_ast(text_document.clone()).await;
    }

    async fn update_ast(&self, text_document: TextDocumentItem) {
        let file_id = SourceFileId::dummy_file();
        let (module_ast, _) = fe_parser::parse_file(file_id, &text_document.text);
        self.ast_map
            .insert(text_document.uri.to_string(), module_ast);
    }
}

pub async fn lsp_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        ast_map: DashMap::new(),
        content: DashMap::new()
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}



#[derive(Clone, Copy, PartialEq)]
struct FileId<'a> {
    pub name: &'a str,
    pub source: &'a str
}

impl<'a> FileId<'a> {
    pub fn new(name: &'a str, source: &'a str) -> Self{
        FileId { 
            name,
            source 
        }
    }
}

impl<'a> codespan_reporting::files::Files<'a> for FileId<'_> {
    type FileId = FileId<'a>;
    type Name = Rc<str>;
    type Source = Rc<str>;

    fn name(&self, file: FileId) -> Result<Self::Name, CsError> {
        return Ok(Rc::from(file.name));
    }

    fn source(&self, file: FileId) -> Result<Self::Source, CsError> {
        return Ok(Rc::from(file.source));
    }

    fn line_index(&self, file: FileId, byte_index: usize) -> Result<usize, CsError> {
        let src = file.source.to_string();
        let line_idx = Rope::from_str(&src).char_to_line(byte_index);
        Ok(line_idx)
    }

    fn line_range(&self, file: FileId, line_index: usize) -> Result<std::ops::Range<usize>, CsError> {
        let src = file.source.to_string();
        
        let r = Rope::from_str(&src);
        let mut start = 0;
        let mut end = 0;
        if line_index == 0 {
            return Ok(std::ops::Range{start: 0, end: r.line(0).to_string().len()});
        }
        for i in 0..line_index{
            start += r.line(i).to_string().len();
        }
        end = start + r.line(line_index).to_string().len();
        Ok(std::ops::Range{start, end})
    }
}