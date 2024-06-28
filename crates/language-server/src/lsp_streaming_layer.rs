// //! # async-lsp-streams
// //!
// //! This crate provides an extension to the `async-lsp` library, allowing easy creation of
// //! stream-based handlers for LSP requests and notifications.

// use async_lsp::router::Router;
// use async_lsp::{lsp_types::*, ResponseError};
// use futures::Stream;
// use notification::Notification;
// use request::Request;
// use std::pin::Pin;
// use std::task::{Context, Poll};
// use tokio::sync::{mpsc, oneshot};
// use tower::Layer;

// use std::marker::PhantomData;

// /// A stream of LSP request messages with their response channels.
// pub struct RequestStream<Params, Result> {
//     receiver: mpsc::Receiver<(
//         Params,
//         oneshot::Sender<std::result::Result<Result, ResponseError>>,
//     )>,
// }

// struct RequestStreamSender<R: Request> {
//     sender: mpsc::Sender<(
//         R::Params,
//         oneshot::Sender<std::result::Result<R::Result, ResponseError>>,
//     )>,
//     _phantom: PhantomData<R>,
// }

// impl<Params, Result> Stream for RequestStream<Params, Result> {
//     type Item = (
//         Params,
//         oneshot::Sender<std::result::Result<Result, ResponseError>>,
//     );

//     fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
//         self.receiver.poll_recv(cx)
//     }
// }

// /// A stream of LSP notification messages.
// pub struct NotificationStream<Params> {
//     receiver: mpsc::Receiver<Params>,
// }

// struct NotificationStreamSender<N: Notification> {
//     sender: mpsc::Sender<N::Params>,
//     _phantom: PhantomData<N>,
// }

// impl<Params> Stream for NotificationStream<Params> {
//     type Item = Params;

//     fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
//         self.receiver.poll_recv(cx)
//     }
// }

// /// An extension trait for `RouterBuilder` to add stream-based handlers.
// pub trait LspStreamer {
//     /// Creates a stream for handling a specific LSP request.
//     fn request_stream<R>(&mut self) -> RequestStream<R::Params, R::Result>
//     where
//         R: request::Request;

//     /// Creates a stream for handling a specific LSP notification.
//     fn notification_stream<N>(&mut self) -> NotificationStream<N::Params>
//     where
//         N: notification::Notification;
// }

// pub struct StreamingLayer {}

// impl StreamingLayer {
//     pub fn new() -> Self {
//         Self
//     }
// }

// impl<Router> Layer<Router> for StreamingLayer {
//     type Service = Router;

//     fn layer(&self, inner: Router) -> Self::Service {
//         inner
//     }
// }

// // impl<State> LspStreamer for Router<State> {
// //     fn request_stream<R>(&mut self) -> RequestStream<R::Params, R::Result>
// //     where
// //         R: request::Request,
// //     {
// //         let (tx, rx) = mpsc::channel(100);
// //         self.request::<R, _>(move |_, params| {
// //             let tx = tx.clone();
// //             async move {
// //                 let (response_tx, response_rx) = oneshot::channel();
// //                 tx.send((params, response_tx)).await.unwrap();
// //                 response_rx.await.unwrap()
// //             }
// //         });
// //         RequestStream { receiver: rx }
// //     }

// //     fn notification_stream<N>(&mut self) -> NotificationStream<N::Params>
// //     where
// //         N: notification::Notification,
// //     {
// //         let (tx, rx) = mpsc::channel(100);
// //         self.notification::<N>(move |_, params| {
// //             let tx = tx.clone();
// //             tokio::spawn(async move {
// //                 tx.send(params).await.unwrap();
// //             });
// //             std::ops::ControlFlow::Continue(())
// //         });
// //         NotificationStream { receiver: rx }
// //     }
// // }
