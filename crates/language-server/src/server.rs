use super::state::ServerState;
use anyhow::Result;
use lsp_server::{Connection, Notification};
use lsp_types::{ServerCapabilities, HoverProviderCapability};

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..Default::default()
    }
}

pub fn run_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let (request_id, initialize_params) = connection.initialize_start()?;
    //
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
    connection.sender.send(
        lsp_server::Message::Notification(Notification {
            method: String::from("window/showMessage"),
            params: serde_json::to_value(lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::INFO,
                message: String::from("hello from the Fe language server"),
            }).unwrap()
        })
    )?;
    
    // log a startup message
    connection.sender.send(
        lsp_server::Message::Notification(Notification {
            method: String::from("window/logMessage"),
            params: serde_json::to_value(lsp_types::LogMessageParams {
                typ: lsp_types::MessageType::INFO,
                message: String::from("Fe language server started"),
            }).unwrap()
        })
    )?;

    io_threads.join().unwrap();

    ServerState::new(connection.sender).run(connection.receiver)
}
