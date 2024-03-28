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

pub async fn setup_streams(backend: &mut Backend, mut receivers: MessageReceivers) {
    info!("setting up streams");
    let mut initialized_stream = receivers.initialize_stream.fuse();
    let mut shutdown_stream = receivers.shutdown_stream.fuse();
    let did_change_watched_files_stream = receivers.did_change_watched_files_stream.fork();

    let flat_did_change_watched_files = did_change_watched_files_stream
        .map(|params| futures::stream::iter(params.changes))
        .flatten()
        .fork();

    let did_change_watched_file_stream = flat_did_change_watched_files.clone().filter(|change| {
        let change_type = change.typ;
        Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::CHANGED) })
    });

    let did_create_watched_file_stream = flat_did_change_watched_files.clone().filter(|change| {
        let change_type = change.typ;
        Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::CREATED) })
    });

    let mut did_delete_watch_file_stream = flat_did_change_watched_files
        .clone()
        .filter(|change| {
            let change_type = change.typ;
            Box::pin(async move { matches!(change_type, lsp_types::FileChangeType::DELETED) })
        })
        .fuse();

    let did_open_stream = (&mut receivers.did_open_stream).fuse();
    let did_change_stream = (&mut receivers.did_change_stream).fuse();
    let mut change_stream = (
        did_change_watched_file_stream.map(|change| {
            let uri = change.uri;
            let path = uri.to_file_path().unwrap();
            let text = std::fs::read_to_string(path).unwrap();
            TextDocumentItem {
                uri: uri.clone(),
                language_id: LANGUAGE_ID.to_string(),
                version: 0,
                text,
            }
        }),
        did_create_watched_file_stream.map(|change| {
            let uri = change.uri;
            let path = uri.to_file_path().unwrap();
            let text = std::fs::read_to_string(path).unwrap();
            TextDocumentItem {
                uri: uri.clone(),
                language_id: LANGUAGE_ID.to_string(),
                version: 0,
                text,
            }
        }),
        did_open_stream.map(|params| TextDocumentItem {
            uri: params.text_document.uri,
            language_id: LANGUAGE_ID.to_string(),
            version: params.text_document.version,
            text: params.text_document.text,
        }),
        did_change_stream.map(|params| TextDocumentItem {
            uri: params.text_document.uri,
            language_id: LANGUAGE_ID.to_string(),
            version: params.text_document.version,
            text: params.content_changes[0].text.clone(),
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
            Some(params) = did_delete_watch_file_stream.next() => backend.handle_deleted(params, tx_needs_diagnostics.clone()).await,
            Some(params) = change_stream.next() => backend.handle_change(params, tx_needs_diagnostics.clone()).await,
            Some(files_need_diagnostics) = diagnostics_stream.next() => backend.handle_diagnostics(files_need_diagnostics).await,
            Some((params, responder)) = hover_stream.next() => backend.handle_hover(params, responder).await,
            Some((params, responder)) = goto_definition_stream.next() => backend.handle_goto_definition(params, responder).await,
        }
        tokio::task::yield_now().await;
    }
}
