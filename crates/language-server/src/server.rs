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
    info!("Setting up server");
    let client_worker_thread = client.clone();
    let actor_ref = Actor::spawn(move || Ok(Backend::new(client_worker_thread)));
    let mut dispatcher = LspDispatcher::new();

    let streaming_router = Router::new(());

    let mut backup_service = Router::new(());

    let got_actor_ref = actor_ref.is_ok();
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
            if got_actor_ref {
                info!("Somehow managed to initialize the actor even though we're in fallback mode.")
            } else {
                error!("Failed to initialize the actor!")
            }

            ControlFlow::Continue(())
        });

    let mut services = match actor_ref {
        Ok(actor_ref) => {
            HandlerRegistration {
                actor_ref: &actor_ref,
                dispatcher: &mut dispatcher,
            }
            .handle_request::<Initialize>(handlers::initialize)
            .handle_notification::<Initialized>(handlers::initialized)
            .handle_request::<HoverRequest>(handlers::handle_hover_request);

            let actor_service = LspActorService::new(actor_ref.clone(), dispatcher);
            vec![BoxLspService::new(actor_service)]
        }
        Err(_) => Vec::new(),
    };
    services.extend([
        BoxLspService::new(streaming_router),
        BoxLspService::new(backup_service),
    ]);

    let router = LspSteer::new(services, FirstComeFirstServe);
    BoxLspService::new(router)
}
