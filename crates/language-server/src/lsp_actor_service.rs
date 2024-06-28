use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};

use async_lsp::{AnyEvent, AnyNotification, AnyRequest, Error, LspService, ResponseError};
use tower::Service;

use crate::actor::{ActorError, ActorRef};

pub struct LspActorService {
    actor_ref: ActorRef,
}

impl LspActorService {
    pub fn new(actor_ref: ActorRef) -> Self {
        Self { actor_ref }
    }
}

impl Service<AnyRequest> for LspActorService {
    type Response = serde_json::Value;
    type Error = ResponseError;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, ResponseError>> + Send>>;

    fn poll_ready(&mut self, _cx: &mut Context<'_>) -> Poll<Result<(), ResponseError>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        let actor_ref = self.actor_ref.clone();
        Box::pin(async move {
            actor_ref
                .ask::<AnyRequest, serde_json::Value>(req)
                .await
                .map_err(|e| match e {
                    ActorError::HandlerNotFound => ResponseError::new(
                        async_lsp::ErrorCode::METHOD_NOT_FOUND,
                        "Method not found".to_string(),
                    ),
                    _ => {
                        ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("{:?}", e))
                    }
                })
        })
    }
}

impl LspService for LspActorService {
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        let method = notif.method.clone();
        match self.actor_ref.tell(notif) {
            Ok(()) => ControlFlow::Continue(()),
            Err(ActorError::HandlerNotFound) => {
                tracing::warn!("Method not found for notification `{}`", method);
                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Response(ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!(
                    "Failed to send notification: {:?} for notification `{}`",
                    e, method
                ),
            )))),
        }
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        let type_name = event.type_name();
        match self.actor_ref.tell(event) {
            Ok(()) => ControlFlow::Continue(()),
            Err(ActorError::HandlerNotFound) => {
                tracing::warn!("Method not found for event: {:?}", type_name);
                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Response(ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("Failed to emit event: {:?}", e),
            )))),
        }
    }
}
