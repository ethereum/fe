use super::state::ServerState;
use anyhow::Result;
use lsp_server::{Connection, Notification};
use lsp_types::{HoverProviderCapability, InitializeParams, ServerCapabilities};

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // full sync mode for now
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::FULL,
        )),
        // goto definition
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        // support for workspace add/remove changes
        ..Default::default()
    }
}

pub fn run_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let (request_id, _initialize_params) = connection.initialize_start()?;
    let initialize_params: InitializeParams = serde_json::from_value(_initialize_params)?;
    // todo: actually use initialization params

    let capabilities = server_capabilities();

    let initialize_result = lsp_types::InitializeResult {
        capabilities: capabilities,
        server_info: Some(lsp_types::ServerInfo {
            name: String::from("fe-language-server"),
            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
        }),
    };

    let initialize_result = serde_json::to_value(initialize_result).unwrap();

    connection.initialize_finish(request_id, initialize_result)?;
    // send a "hello" message to the client
    connection
        .sender
        .send(lsp_server::Message::Notification(Notification {
            method: String::from("window/showMessage"),
            params: serde_json::to_value(lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::INFO,
                message: String::from("hello from the Fe language server"),
            })
            .unwrap(),
        }))?;

    let mut state = ServerState::new(connection.sender);
    let _ = state.init_logger(log::Level::Info);
    state.workspace.set_workspace_root(
        &mut state.db,
        &initialize_params.root_uri.unwrap().to_file_path().ok(),
    );
    let result = state.run(connection.receiver)?;

    io_threads.join().unwrap();

    Ok(result)
}
