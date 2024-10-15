use std::ops::ControlFlow;

use crate::functionality::handlers::{FileChange, FilesNeedDiagnostics};
use crate::functionality::streams::setup_streams;
use crate::logger::ClientSocketWriterMaker;
use crate::lsp_actor::{LspActor, LspDispatcher};
use crate::lsp_actor_service::LspActorService;
use act_locally::builder::ActorBuilder;
use async_lsp::lsp_types::notification::{
    self, DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument, Initialized,
};
use async_lsp::lsp_types::request::{GotoDefinition, HoverRequest};
use async_lsp::lsp_types::{request, InitializeResult, ServerCapabilities};
use async_lsp::ClientSocket;
use serde_json::Value;
use tracing::subscriber::set_default;
use tracing::{error, info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

use crate::backend::Backend;
use crate::functionality::{goto, handlers};
use act_locally::actor::HandlerRegistration;
use async_lsp::{
    lsp_types::request::Initialize,
    router::Router,
    steer::{FirstComeFirstServe, LspSteer},
    util::BoxLspService,
    ResponseError,
};

pub(crate) struct Lol;

// use tracing_subscriber::Layer;

fn setup_logger(
    client: ClientSocket,
) -> impl FnOnce() -> Option<tracing::subscriber::DefaultGuard> {
    move || {
        let client_socket_writer = ClientSocketWriterMaker::new(client);
        let subscriber = tracing_subscriber::registry()
            .with(
                HierarchicalLayer::new(2)
                    .with_thread_ids(true)
                    .with_thread_names(true)
                    .with_indent_lines(true)
                    .with_bracketed_fields(true)
                    .with_ansi(false)
                    .with_writer(client_socket_writer),
            )
            .with(
                HierarchicalLayer::new(2)
                    .with_thread_ids(true)
                    .with_thread_names(true)
                    .with_indent_lines(true)
                    .with_bracketed_fields(true)
                    .with_writer(std::io::stderr),
            );
        Some(set_default(subscriber))
    }
}

pub(crate) fn setup(client: ClientSocket, name: String) -> BoxLspService<Value, ResponseError> {
    info!("Setting up server");
    let client_worker_thread = client.clone();
    let client_logger = client.clone();
    let actor_ref = ActorBuilder::new()
        .with_name(name)
        .with_state_init(move || Ok(Backend::new(client_worker_thread)))
        .with_subscriber_init(setup_logger(client_logger))
        .spawn();
    let mut dispatcher = LspDispatcher::new();

    let mut streaming_router = Router::new(());

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

    // event handler test
    // async fn handle_lol(_: &mut Backend, _: Lol) -> Result<(), ResponseError> {
    //     warn!("RECEIVED AN LOL!");
    //     Ok(())
    // }

    let mut services = match actor_ref {
        Ok(actor_ref) => {
            HandlerRegistration {
                actor_ref: &actor_ref,
                dispatcher: &mut dispatcher,
            }
            // .handle_event::<Lol>(handle_lol)
            .handle_request::<Initialize>(handlers::initialize)
            .handle_notification::<Initialized>(handlers::initialized)
            .handle_request::<GotoDefinition>(goto::handle_goto_definition)
            .handle_request::<HoverRequest>(handlers::handle_hover_request)
            .handle_notification::<DidOpenTextDocument>(handlers::handle_did_open_text_document)
            .handle_notification::<DidChangeTextDocument>(handlers::handle_did_change_text_document)
            .handle_notification::<DidChangeWatchedFiles>(handlers::handle_did_change_watched_files)
            .handle_event::<FileChange>(handlers::handle_file_change)
            .handle_event::<FilesNeedDiagnostics>(handlers::handle_files_need_diagnostics)
            .handle_notification::<notification::Exit>(handlers::handle_exit);

            setup_streams(client.clone(), &mut streaming_router, actor_ref.clone());

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
