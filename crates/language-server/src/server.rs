use anyhow::Result;
use lsp_server::Connection;
use super::state::ServerState;

pub fn run_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    
    let result = ServerState::new(connection.sender)
        .run(connection.receiver);   

    io_threads.join().unwrap();
    
    result
}