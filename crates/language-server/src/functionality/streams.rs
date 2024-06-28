use std::ops::ControlFlow;

use crate::functionality::handlers::FilesNeedDiagnostics;
use crate::lsp_actor::{ActOnNotification, ActOnRequest};
use crate::lsp_streams::RouterStreams;
// use crate::lsp_kameo::RouterActors;
use async_lsp::lsp_types::request::Initialize;
use async_lsp::lsp_types::{notification, request, InitializeParams, InitializeResult};
use async_lsp::router::Router;
use async_lsp::{lsp_types, ClientSocket, ResponseError};
use futures::future::join_all;
use futures::stream::FuturesUnordered;
use futures::{join, StreamExt};
use futures_batch::ChunksTimeoutStreamExt;
use futures_concurrency::prelude::*;
// use kameo::actor::ActorRef;
// use kameo::error::SendError;
use lsp_types::FileChangeType;
use tokio_stream::wrappers::UnboundedReceiverStream;

use tracing::info;

use crate::actor::ActorRef;

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

pub async fn setup_streams(
    // backend: &mut Backend,
    router: &mut Router<ActorRef>,
    actor: ActorRef,
) {
    info!("setting up streams");

    let did_change_watched_files_stream = router
        .notification_stream::<notification::DidChangeWatchedFiles>()
        .fuse();

    // backend.attach_stream(did_change_watched_files_stream, (), ());

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

    // We can't do this because there's no way of returning a local future from a closure
    // router.request::<request::Initialize, _>(|st: &mut Backend, params: InitializeParams| {
    //     async move {
    //         // I want to use the backend here, but I can't because it's not `Sync` or `Send` and I don't want it to be or to use locks...
    //         st.db;
    //         Ok(InitializeResult::default())
    //     }
    // });
    // router.request_to_actor::<request::Initialize>(backend.clone());

    let initialize_stream = router.request_stream::<request::Initialize>().fuse();
    // .for_each(|(params, response_tx)| {
    //     let backend = backend.clone();
    //     async move {
    //         let result = backend.ask(params).send().await;
    //         let _ = response_tx.send(flatten_result(result));
    //     }
    // });

    // backend.attach_stream(initialize_stream, (), ());
    router.act_on_request::<Initialize>(&actor);
    router.act_on_notification::<async_lsp::lsp_types::notification::DidChangeWatchedFiles>(&actor);

    let hover_stream = router.request_stream::<request::HoverRequest>().fuse();
    // .for_each(|(params, response_tx)| {
    //     let backend = backend.clone();
    //     async move {
    //         let result = backend.ask(params).send().await;
    //         let _ = response_tx.send(flatten_result(result));
    //     }
    // });

    // let change_handler = change_stream
    //     .for_each(|change| {
    //         let uri = change.uri.to_string();
    //         backend.ask(change).send();
    //         tx_needs_diagnostics.send(uri).unwrap();
    //         futures::future::ready(())
    //     });

    // let diagnostics_handler = diagnostics_stream
    //     .for_each(|files_need_diagnostics| {
    //         backend.ask(files_need_diagnostics).send();
    //         futures::future::ready(())
    //     });

    // join_all(vec![
    //     initialize_stream,
    //     hover_stream,
    //     change_handler,
    //     diagnostics_handler,
    // ]);

    // info!("streams set up, looping on them now");
    // loop {
    //     tokio::select! {
    //         Some(change) = change_stream.next() => {
    //             let uri = change.uri.to_string();
    //             backend.send(change);
    //             tx_needs_diagnostics.send(uri).unwrap();
    //         },
    //         Some(files_need_diagnostics) = diagnostics_stream.next() => {
    //             backend.ask(files_need_diagnostics).send();
    //         },
    //     }
    //     tokio::task::yield_now().await;
    // }
}

// trait IntoResponseError {
//     fn into_response_error(self) -> ResponseError;
// }

// // Implement the trait for SendError
// impl<T> IntoResponseError for SendError<T, ResponseError>
// where
//     T: Send + 'static,
// {
//     fn into_response_error(self) -> ResponseError {
//         ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, self.to_string())
//     }
// }

// // Helper function to flatten nested Results
// fn flatten_result<T, E>(result: Result<T, E>) -> Result<T, ResponseError>
// where
//     E: IntoResponseError,
// {
//     result.map_err(|e| e.into_response_error())
// }

// // backend stream handling
// use futures::Stream;
// // use std::pin::Pin;
// use tokio::task::JoinHandle;

// use super::actor::ActorRef;

// pub trait StreamHandler {
//     fn add_stream<S, F, Fut>(&mut self, stream: S, handler: F) -> JoinHandle<()>
//     where
//         S: Stream + Send + 'static,
//         F: FnMut(S::Item) -> Fut + Send + 'static,
//         Fut: std::future::Future<Output = ()> + Send + 'static;
// }

// impl StreamHandler for Backend {
//     fn add_stream<S, F, Fut>(&mut self, stream: S, mut handler: F) -> JoinHandle<()>
//     where
//         S: Stream + Send + 'static,
//         F: FnMut(S::Item) -> Fut + Send + 'static,
//         Fut: std::future::Future<Output = ()> + Send + 'static,
//     {
//         tokio::spawn(async move {
//             // use futures::StreamExt;
//             let mut pinned = Box::pin(stream);
//             while let Some(item) = pinned.next().await {
//                 handler(item).await;
//             }
//         })
//     }
// }
