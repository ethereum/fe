mod backend;
mod functionality;
mod lsp_actor;
mod lsp_actor_service;
mod lsp_streaming_layer;
mod lsp_streams;
mod server;
// mod streaming_router;
mod util;

use std::borrow::Borrow;
use std::ops::ControlFlow;

use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::notification::{self, Initialized, LogMessage};
use async_lsp::lsp_types::request::HoverRequest;
use async_lsp::lsp_types::{
    request, Hover, HoverProviderCapability, InitializeResult, LogMessageParams, OneOf,
    ServerCapabilities,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_lsp::{LanguageClient, LspService};
use lsp_actor::{LspActor, LspDispatcher};
use lsp_actor_service::LspActorService;
// use lsp_actor_service::LspActorService;
use tracing::Level;
use tracing::{error, info};

use act_locally::actor::{Actor, HandlerRegistration};
use async_lsp::{
    client_monitor::ClientProcessMonitorLayer,
    lsp_types::request::Initialize,
    router::Router,
    steer::{FirstComeFirstServe, LspSteer},
    util::BoxLspService,
    ResponseError,
};
use backend::{db::Jar, Backend};
use functionality::handlers;
// use lsp_actor::{ActOnNotification, ActOnRequest};
use tower::ServiceBuilder;
struct TickEvent;

// impl<M> CanHandle<M> for LspActorService {
//     fn can_handle(&self, msg: &M) -> bool {
//         true
//     }
// }

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let client_worker_thread = client.clone();
        let (actor_ref, dispatcher) = Actor::spawn_local(move || {
            let backend = Backend::new(client_worker_thread);
            let (mut actor, actor_ref) = Actor::new(backend);
            let mut dispatcher = LspDispatcher::new();

            HandlerRegistration {
                actor: &mut actor,
                dispatcher: &mut dispatcher,
            }
            .handle_request::<Initialize>(handlers::initialize)
            .handle_notification::<Initialized>(handlers::initialized)
            // .handle_request::<HoverRequest>(handlers::handle_hover_request)
            // .handle_request::<Ho
            ;

            Ok((actor, actor_ref, dispatcher))
        })
        .ok()
        .unzip();

        let streaming_router = Router::new(());

        let mut backup_service = Router::new(());

        let got_actor_ref = actor_ref.is_some();
        let got_dispatcher = dispatcher.is_some();
        backup_service
            .request::<request::Initialize, _>(|_, _| async move {
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        ..ServerCapabilities::default()
                    },
                    server_info: None,
                })
            })
            .notification::<notification::Initialized>(move |_, _| {
                info!("Entering fallback mode: something is broken.");
                if got_actor_ref && got_dispatcher {
                    info!("Somehow managed to initialize the actor even though we're in fallback mode.")
                } else {
                    error!("Failed to initialize the actor!")
                }

                ControlFlow::Continue(())
            });

        let mut services = match actor_ref {
            Some(actor_ref) => {
                let actor_service = LspActorService::new(actor_ref.clone(), dispatcher.unwrap());
                vec![BoxLspService::new(actor_service)]
            }
            None => Vec::new(),
        };
        services.extend([
            BoxLspService::new(streaming_router),
            BoxLspService::new(backup_service),
        ]);

        let router = LspSteer::new(services, FirstComeFirstServe);
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
