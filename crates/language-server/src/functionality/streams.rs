use crate::backend::Backend;
use crate::functionality::handlers::{
    handle_file_change, handle_files_need_diagnostics, ChangeKind, FileChange,
    FilesNeedDiagnostics, NeedsDiagnostics,
};
use crate::lsp_actor_service::LspActorKey;
use crate::lsp_streams::RouterStreams;
use act_locally::dispatcher::GenericDispatcher;
use act_locally::types::ActorError;
use async_lsp::lsp_types::notification;
use async_lsp::router::Router;
use async_lsp::{lsp_types, ClientSocket, LspService};
use futures::StreamExt;
use futures_batch::ChunksTimeoutStreamExt;
use futures_concurrency::prelude::*;
use lsp_types::FileChangeType;
use tokio_stream::wrappers::UnboundedReceiverStream;
use tracing::instrument::WithSubscriber;

use tracing::info;

use act_locally::actor::ActorRef;

pub fn setup_streams(
    client: ClientSocket,
    router: &mut Router<()>,
    backend: ActorRef<Backend, LspActorKey>,
) {
    info!("setting up streams");

    // backend.register_handler_async_mutating(
    //     LspActorKey::of::<FilesNeedDiagnostics>().into(),
    //     handle_files_need_diagnostics,
    // );

    // backend.register_handler_async_mutating(
    //     LspActorKey::of::<FileChange>().into(),
    //     handle_file_change,
    // );

    // let (tx_needs_diagnostics, rx_needs_diagnostics) =
    //     tokio::sync::mpsc::unbounded_channel::<FileNeedsDiagnostics>();

    let mut diagnostics_stream = router
        .event_stream::<NeedsDiagnostics>()
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .map(FilesNeedDiagnostics)
        .fuse();

    tokio::spawn(
        async move {
            loop {
                tokio::select! {
                    // Some(change) = change_stream.next() => {
                    //     let uri = change.uri.path().to_string();
                    //     let _ = &client.emit(change);
                    //     let _ = tx_needs_diagnostics.send(uri);
                    // },
                    Some(files_need_diagnostics) = diagnostics_stream.next() => {
                        let _ = &client.emit(files_need_diagnostics);
                    },
                }
                tokio::task::yield_now().await;
            }
        }
        .with_current_subscriber(),
    );
}
