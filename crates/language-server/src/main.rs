mod backend;
mod cli_args;
mod functionality;
mod lsp_actor;
mod lsp_actor_service;
mod lsp_streaming_layer;
mod lsp_streams;
mod server;
// mod streaming_router;
mod util;

use std::backtrace::Backtrace;
use std::net::SocketAddr;

use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use clap::Parser;
use cli_args::{CliArgs, Commands};
use futures::io::{AsyncReadExt};
use futures::StreamExt;
use futures_net::TcpListener;
use server::setup;
// use lsp_actor_service::LspActorService;
use tracing::Level;
use tracing::{error, info};

use async_lsp::client_monitor::ClientProcessMonitorLayer;
use backend::db::Jar;
// use lsp_actor::{ActOnNotification, ActOnRequest};
use tower::ServiceBuilder;


#[tokio::main]
async fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    // Set up a panic hook
    std::panic::set_hook(Box::new(|panic_info| {
        // Extract the panic message
        let payload = panic_info.payload();
        let message = if let Some(s) = payload.downcast_ref::<&str>() {
            *s
        } else if let Some(s) = payload.downcast_ref::<String>() {
            &s[..]
        } else {
            "Unknown panic message"
        };

        // Get the location of the panic if available
        let location = if let Some(location) = panic_info.location() {
            format!(" at {}:{}", location.file(), location.line())
        } else {
            String::from("Unknown location")
        };

        // Capture the backtrace
        let backtrace = Backtrace::capture();

        // Log the panic information and backtrace
        tracing::error!(
            "Panic occurred{}: {}\nBacktrace:\n{:?}",
            location,
            message,
            backtrace
        );
    }));

    // Parse CLI arguments
    let args = CliArgs::parse();

    match args.command {
        Some(Commands::Tcp(tcp_args)) => {
            // Start server with TCP listener
            start_tcp_server(tcp_args.port).await;
        }
        None => {
            // Start server with stdio
            start_stdio_server().await;
        }
    }
}

async fn start_stdio_server() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let router = setup(client.clone());
        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(router)
    });

    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );

    // Fallback to spawn blocking read/write otherwise.
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    match server.run_buffered(stdin, stdout).await {
        Ok(_) => info!("Server finished successfully"),
        Err(e) => error!("Server error: {:?}", e),
    }
}

async fn start_tcp_server(port: u16) {
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let mut listener = TcpListener::bind(&addr)
        // .await
        .expect("Failed to bind to address");
    let mut incoming = listener.incoming();

    // let listener = stream.
    info!("LSP server is listening on {}", addr);

    while let Some(Ok(stream)) = incoming.next().await {
        // info!("New client connected from {}", client_addr);
        // Spawn a new task for each client connection
        let client_address = stream.peer_addr().unwrap();
        tokio::spawn(async move {
            let (server, _) = async_lsp::MainLoop::new_server(|client| {
                let router = setup(client.clone());
                ServiceBuilder::new()
                    .layer(TracingLayer::default())
                    .layer(LifecycleLayer::default())
                    .layer(CatchUnwindLayer::default())
                    .layer(ConcurrencyLayer::default())
                    .layer(ClientProcessMonitorLayer::new(client.clone()))
                    .service(router)
            });

            let (read, write) = stream.split();

            if let Err(e) = server.run_buffered(read, write).await {
                error!("Server error for client {}: {:?}", client_address, e);
            } else {
                info!("Client {} disconnected", client_address);
            }
        });
    }
}
