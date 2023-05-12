use anyhow::Result;
use lsp_server::Connection;
use lsp_types::ServerCapabilities;
use super::state::ServerState;

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        ..Default::default()
    }
}

pub fn run_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    
    let (request_id, initialize_params) = connection.initialize_start()?;
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

    io_threads.join().unwrap();
    
    ServerState::new(connection.sender)
        .run(connection.receiver)
}