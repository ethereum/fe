// use async_lsp::router::Router;
// use async_lsp::{AnyEvent, AnyNotification, AnyRequest, LspService, ResponseError, Result};
// use serde_json::Value as JsonValue;
// use std::future::Future;
// use std::ops::ControlFlow;
// use std::pin::Pin;
// use std::task::{Context, Poll};
// use tower::Service;

// pub struct RouterService<St, InnerService> {
//     inner: InnerService,
// }

// impl<St, InnerService> Service<AnyRequest> for RouterService<St, InnerService>
// where
//     St: Default + Send + 'static,
//     InnerService: Service<
//             AnyRequest,
//             Response = JsonValue,
//             Error = ResponseError,
//             Future = Pin<Box<dyn Future<Output = Result<JsonValue, ResponseError>> + Send>>,
//         > + Send
//         + 'static,
// {
//     type Response = JsonValue;
//     type Error = ResponseError;
//     type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

//     fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
//         self.inner.poll_ready(cx)
//     }

//     fn call(&mut self, req: AnyRequest) -> Self::Future {
//         let router_future = self.router.call(req.clone());
//         let inner_future = self.inner.call(req);

//         Box::pin(async move {
//             match router_future.await {
//                 Ok(response) => Ok(response),
//                 Err(_) => inner_future.await,
//             }
//         })
//     }
// }

// impl<St, InnerService> LspService for RouterService<St, InnerService>
// where
//     St: Default + Send + 'static,
//     InnerService: LspService
//         + Service<AnyRequest, Response = JsonValue, Error = ResponseError>
//         + Send
//         + 'static,
// {
//     fn notify(&mut self, notif: AnyNotification) -> ControlFlow<Result<()>> {
//         match self.router.notify(notif.clone()) {
//             ControlFlow::Continue(()) => ControlFlow::Continue(()),
//             ControlFlow::Break(Err(_)) => self.inner.notify(notif),
//             ControlFlow::Break(Ok(())) => ControlFlow::Break(Ok(())),
//         }
//     }

//     fn emit(&mut self, event: AnyEvent) -> ControlFlow<Result<()>> {
//         let router_result = self.router.emit(event);
//         match router_result {
//             ControlFlow::Continue(()) => ControlFlow::Continue(()),
//             ControlFlow::Break(Err(_)) => self.inner.emit(event),
//             ControlFlow::Break(Ok(())) => ControlFlow::Break(Ok(())),
//         }
//     }
// }
