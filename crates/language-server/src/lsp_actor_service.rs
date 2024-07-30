use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::sync::{Arc, Weak};
use std::task::{Context, Poll};

use async_lsp::can_handle::CanHandle;
use async_lsp::{AnyEvent, AnyNotification, AnyRequest, Error, LspService, ResponseError};
use tower::Service;

use crate::actor::{ActorError, ActorRef};
use crate::lsp_actor::LspDispatcher;

pub struct LspActorService {
    actor_ref: ActorRef,
    dispatcher: Arc<LspDispatcher>,
}

impl LspActorService {
    pub fn new(actor_ref: ActorRef, dispatcher: LspDispatcher) -> Self {
        Self {
            actor_ref,
            dispatcher: Arc::new(dispatcher),
        }
    }
}

impl Service<AnyRequest> for LspActorService {
    type Response = serde_json::Value;
    type Error = ResponseError;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(&mut self, _cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        let actor_ref = self.actor_ref.clone();
        let dispatcher = self.dispatcher.clone();
        Box::pin(async move {
            let dispatcher = dispatcher.as_ref();
            let ask = actor_ref.ask(dispatcher, req);
            ask.await.map_err(|e| match e {
                ActorError::HandlerNotFound => ResponseError::new(
                    async_lsp::ErrorCode::METHOD_NOT_FOUND,
                    "Method not found".to_string(),
                ),
                _ => ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, format!("{:?}", e)),
            })
        })
    }
}

impl LspService for LspActorService {
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        let method = notif.method.clone();
        let dispatcher = self.dispatcher.clone();
        match self.actor_ref.tell(dispatcher.as_ref(), notif) {
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
        let dispatcher = self.dispatcher.clone();
        match self.actor_ref.tell(dispatcher.as_ref(), event) {
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

impl CanHandle<AnyRequest> for LspActorService {
    fn can_handle(&self, req: &AnyRequest) -> bool {
        self.dispatcher.as_ref().dispatch_request(req).is_ok()
    }
}

impl CanHandle<AnyNotification> for LspActorService {
    fn can_handle(&self, notif: &AnyNotification) -> bool {
        self.dispatcher
            .as_ref()
            .dispatch_notification(notif)
            .is_ok()
    }
}

impl CanHandle<AnyEvent> for LspActorService {
    fn can_handle(&self, _: &AnyEvent) -> bool {
        false
    }
}
