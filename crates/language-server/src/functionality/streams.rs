use crate::lsp_actor::LspDispatcher;
use crate::backend::Backend;
use crate::lsp_streams::RouterStreams;
use act_locally::dispatcher::Dispatcher;
use async_lsp::lsp_types;
use async_lsp::lsp_types::{notification, request};
use async_lsp::router::Router;
use futures::{Stream, StreamExt};
use futures_batch::ChunksTimeoutStreamExt;
use futures_concurrency::prelude::*;
use lsp_types::FileChangeType;
use tokio_stream::wrappers::UnboundedReceiverStream;

use tracing::info;

use act_locally::actor::ActorRef;

pub struct FileChange {
    pub uri: url::Url,
    pub kind: ChangeKind,
}
pub enum ChangeKind {
    Open(String),
    Create,
    Edit(Option<String>),
    Delete,
}

pub fn setup_streams(router: &mut Router<()>, backend: ActorRef<Backend, String>) {
    info!("setting up streams");

    let did_change_watched_files_stream = router
        .notification_stream::<notification::DidChangeWatchedFiles>()
        .fuse();

    let did_open_stream = router
        .notification_stream::<notification::DidOpenTextDocument>()
        .fuse();
    let did_change_stream = router
        .notification_stream::<notification::DidChangeTextDocument>()
        .fuse();
    let (tx_needs_diagnostics, rx_needs_diagnostics) =
        tokio::sync::mpsc::unbounded_channel::<String>();
    let mut diagnostics_stream = UnboundedReceiverStream::from(rx_needs_diagnostics)
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .fuse();

    let mut change_stream = (
        did_change_watched_files_stream
            .map(|params| futures::stream::iter(params.changes))
            .flatten()
            .fuse()
            .map(|event| {
                let kind = match event.typ {
                    FileChangeType::CHANGED => ChangeKind::Edit(None),
                    FileChangeType::CREATED => ChangeKind::Create,
                    FileChangeType::DELETED => ChangeKind::Delete,
                    _ => unreachable!(),
                };
                FileChange {
                    uri: event.uri,
                    kind,
                }
            }),
        did_open_stream.fuse().map(|params| FileChange {
            uri: params.text_document.uri,
            kind: ChangeKind::Open(params.text_document.text),
        }),
        did_change_stream.fuse().map(|params| FileChange {
            uri: params.text_document.uri,
            kind: ChangeKind::Edit(Some(params.content_changes[0].text.clone())),
        }),
    )
        .merge()
        .fuse();

    // tokio::spawn(async move {
    //     loop {
    //         tokio::select! {
    //             Some(change) = change_stream.next() => {
    //                 let uri = change.uri.to_string();
    //                 backend.tell(change).await;
    //                 tx_needs_diagnostics.send(uri);
    //             },
    //             Some(files_need_diagnostics) = diagnostics_stream.next() => {
    //                 backend.ask(files_need_diagnostics).await;
    //             },
    //         }
    //         tokio::task::yield_now().await;
    //     }
    // });
}
