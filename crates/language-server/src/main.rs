mod backend;
mod functionality;
mod lsp_actor;
mod lsp_actor_service;
mod lsp_streaming_layer;
mod lsp_streams;
mod server;
// mod streaming_router;
mod util;

use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use futures::stream::StreamExt;
use lsp_actor::{LspActor, LspDispatcher};
// use lsp_actor_service::LspActorService;
use serde_json::Value;
use std::{ops::ControlFlow, sync::Arc, time::Duration};
use tokio::sync::{Mutex, RwLock};
use tracing::Level;
use tracing::{error, info};

use act_locally::actor::{Actor, HandlerRegistration};
use async_lsp::{
    can_handle::CanHandle,
    client_monitor::ClientProcessMonitorLayer,
    lsp_types::{
        notification::Initialized,
        request::{HoverRequest, Initialize, Request},
        Hover, InitializeParams, InitializeResult,
    },
    router::Router,
    steer::{self, FirstComeFirstServe, LspPicker, LspSteer},
    util::BoxLspService,
    AnyEvent, AnyNotification, AnyRequest, ClientSocket, LspService, ResponseError,
};
use backend::{db::Jar, Backend};
use functionality::{handlers, streams::setup_streams};
// use lsp_actor::{ActOnNotification, ActOnRequest};
use lsp_streams::RouterStreams;
use tower::{layer::layer_fn, util::BoxService, Service, ServiceBuilder};
struct TickEvent;

// impl<M> CanHandle<M> for LspActorService {
//     fn can_handle(&self, msg: &M) -> bool {
//         true
//     }
// }

#[tokio::main]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let client_cloned = client.clone();
        let (actor_ref, dispatcher) = Actor::spawn_local(move || {
            let backend = Backend::new(client_cloned);
            let (mut actor, actor_ref) = Actor::new(backend);
            let mut dispatcher = LspDispatcher::new();

            HandlerRegistration {
                actor: &mut actor,
                dispatcher: &mut dispatcher,
            }
            .handle_request::<Initialize>(handlers::initialize);

            (actor, actor_ref, dispatcher)
        });
        let actor_service = lsp_actor_service::LspActorService::new(actor_ref.clone(), dispatcher);

        let mut streaming_router = Router::new(());

        let services: Vec<BoxLspService<serde_json::Value, ResponseError>> = vec![
            BoxLspService::new(streaming_router),
            BoxLspService::new(actor_service),
        ];

        let router = LspSteer::new(services, FirstComeFirstServe);
        ServiceBuilder::new()
            // .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            // .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(router)
    });
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

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
