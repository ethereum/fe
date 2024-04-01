use crate::backend::Backend;
use fork_stream::StreamExt as _;
use futures_batch::ChunksTimeoutStreamExt;

use crate::globals::LANGUAGE_ID;
use crate::server::MessageReceivers;
use futures::StreamExt;
use futures_concurrency::prelude::*;
use lsp_types::TextDocumentItem;
use tokio_stream::wrappers::UnboundedReceiverStream;

use tracing::info;

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

pub async fn setup_streams(backend: &mut Backend, mut receivers: MessageReceivers) {
    info!("setting up streams");
    let mut initialized_stream = receivers.initialize_stream.fuse();
    let mut shutdown_stream = receivers.shutdown_stream.fuse();

    let mut change_stream = (
        receivers
            .did_change_watched_files_stream
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
        receivers.did_open_stream.fuse().map(|params| FileChange {
            uri: params.text_document.uri,
            kind: ChangeKind::Open(params.text_document.text),
        }),
        receivers.did_change_stream.fuse().map(|params| FileChange {
            uri: params.text_document.uri,
            kind: ChangeKind::Edit(Some(params.content_changes[0].text.clone())),
        }),
    )
        .merge()
        .fuse();

    let (tx_needs_diagnostics, rx_needs_diagnostics) =
        tokio::sync::mpsc::unbounded_channel::<String>();

    let mut diagnostics_stream = UnboundedReceiverStream::from(rx_needs_diagnostics)
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .fuse();

    let mut hover_stream = (&mut receivers.hover_stream).fuse();
    let mut goto_definition_stream = (&mut receivers.goto_definition_stream).fuse();

    info!("streams set up, looping on them now");
    loop {
        tokio::select! {
            Some((params, responder)) = initialized_stream.next() => backend.handle_initialized(params, responder).await,
            Some((_, responder)) = shutdown_stream.next() => backend.handle_shutdown(responder).await,
            Some(change) = change_stream.next() => backend.handle_change(change, tx_needs_diagnostics.clone()).await,
            Some(files_need_diagnostics) = diagnostics_stream.next() => backend.handle_diagnostics(files_need_diagnostics).await,
            Some((params, responder)) = hover_stream.next() => backend.handle_hover(params, responder).await,
            Some((params, responder)) = goto_definition_stream.next() => backend.handle_goto_definition(params, responder).await,
        }
        tokio::task::yield_now().await;
    }
}
