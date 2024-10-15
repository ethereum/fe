use std::ops::ControlFlow;

use crate::functionality::handlers::{FileChange, FilesNeedDiagnostics, NeedsDiagnostics};
use crate::logger;
use crate::lsp_actor::{LspActor, LspDispatcher};
use crate::lsp_actor_service::LspActorService;
use crate::lsp_streams::RouterStreams;
use act_locally::builder::ActorBuilder;
use async_lsp::lsp_types::notification::{
    self, DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument, Initialized,
};
use async_lsp::lsp_types::request::{GotoDefinition, HoverRequest};
use async_lsp::lsp_types::{request, InitializeResult, ServerCapabilities};
use async_lsp::ClientSocket;
use async_std::stream::StreamExt;
use futures_batch::ChunksTimeoutStreamExt;
use serde_json::Value;
use tracing::instrument::WithSubscriber;
use tracing::{error, info};

use crate::backend::Backend;
use crate::functionality::{goto, handlers};
use act_locally::actor::HandlerRegistration;
use async_lsp::router::Router;
use async_lsp::{
    lsp_types::request::Initialize,
    steer::{FirstComeFirstServe, LspSteer},
    util::BoxLspService,
    ResponseError,
};

pub(crate) struct Lol;

pub(crate) fn setup(client: ClientSocket, name: String) -> BoxLspService<Value, ResponseError> {
    info!("Setting up server");
    let worker_thread_client = client.clone();
    let logger_client = client.clone();
    let actor_ref = ActorBuilder::new()
        .with_name(name)
        .with_state_init(move || Ok(Backend::new(worker_thread_client)))
        .with_subscriber_init(logger::setup(logger_client))
        .spawn();
    let mut dispatcher = LspDispatcher::new();

    let got_actor_ref = actor_ref.is_ok();
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

            let actor_service = LspActorService::new(actor_ref.clone(), dispatcher);
            vec![BoxLspService::new(actor_service)]
        }
        Err(_) => Vec::new(),
    };

    let mut streaming_router = Router::new(());
    setup_streams(client.clone(), &mut streaming_router);

    let mut backup_service = Router::new(());
    backup_service
        .request::<request::Initialize, _>(|_, _| async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
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

    services.extend([
        BoxLspService::new(streaming_router),
        BoxLspService::new(backup_service),
    ]);

    let router = LspSteer::new(services, FirstComeFirstServe);
    BoxLspService::new(router)
}

pub fn setup_streams(client: ClientSocket, router: &mut Router<()>) {
    info!("setting up streams");

    let mut diagnostics_stream = router
        .event_stream::<NeedsDiagnostics>()
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .map(FilesNeedDiagnostics)
        .fuse();

    tokio::spawn(
        async move {
            while let Some(files_need_diagnostics) = diagnostics_stream.next().await {
                let _ = client.emit(files_need_diagnostics);
            }
        }
        .with_current_subscriber(),
    );
}
