use std::ops::ControlFlow;

use crate::functionality::handlers::{FileChange, FilesNeedDiagnostics, NeedsDiagnostics};
use crate::logging;
use crate::lsp_actor::service::LspActorService;
use crate::lsp_actor::LspActor;
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
    let client_for_actor = client.clone();
    let client_for_logging = client.clone();
    let backend_actor = ActorBuilder::new()
        .with_name(name)
        .with_state_init(move || Ok(Backend::new(client_for_actor)))
        .with_subscriber_init(logging::setup(client_for_logging))
        .spawn();
    let got_actor_ref = backend_actor.is_ok();

    let mut services = match backend_actor {
        Ok(backend_actor_ref) => {
            let mut lsp_actor_service = LspActorService::new(backend_actor_ref.clone());
            lsp_actor_service
                .handle_request::<Initialize>(handlers::initialize)
                .handle_notification::<Initialized>(handlers::initialized)
                .handle_request::<GotoDefinition>(goto::handle_goto_definition)
                .handle_request::<HoverRequest>(handlers::handle_hover_request)
                .handle_notification::<DidOpenTextDocument>(handlers::handle_did_open_text_document)
                .handle_notification::<DidChangeTextDocument>(
                    handlers::handle_did_change_text_document,
                )
                .handle_notification::<DidChangeWatchedFiles>(
                    handlers::handle_did_change_watched_files,
                )
                .handle_event::<FileChange>(handlers::handle_file_change)
                .handle_event::<FilesNeedDiagnostics>(handlers::handle_files_need_diagnostics)
                .handle_notification::<notification::Exit>(handlers::handle_exit);

            vec![BoxLspService::new(lsp_actor_service)]
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
