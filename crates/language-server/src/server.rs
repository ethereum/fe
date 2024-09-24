use std::ops::ControlFlow;

use crate::lsp_actor::{LspActor, LspDispatcher};
use crate::lsp_actor_service::LspActorService;
use async_lsp::lsp_types::notification::{self, Initialized};
use async_lsp::lsp_types::request::HoverRequest;
use async_lsp::lsp_types::{request, InitializeResult, ServerCapabilities};
use async_lsp::ClientSocket;
use serde_json::Value;
// use lsp_actor_service::LspActorService;
use tracing::{error, info};

use crate::backend::Backend;
use crate::functionality::handlers;
use act_locally::actor::{Actor, HandlerRegistration};
use async_lsp::{
    lsp_types::request::Initialize,
    router::Router,
    steer::{FirstComeFirstServe, LspSteer},
    util::BoxLspService,
    ResponseError,
};
// use lsp_actor::{ActOnNotification, ActOnRequest};

pub(crate) fn setup(client: ClientSocket) -> BoxLspService<Value, ResponseError> {
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
        .handle_request::<HoverRequest>(handlers::handle_hover_request);

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
                    // hover_provider: Some(HoverProviderCapability::Simple(true)),
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
    BoxLspService::new(router)
}
